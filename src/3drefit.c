/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

static ATR_float *Update_float_atr(char *aname, char *fvstring);
static ATR_int *Update_int_atr(char *aname, char *ivstring);

void Syntax(int detail)
{
   int ii ;

   printf(
"Changes some of the information inside a 3D dataset's header. ~1~\n"
        "Note that this program does NOT change the .BRIK file at all;\n"
        "the main purpose of 3drefit is to fix up errors made when\n"
        "using to3d.\n"
        "To see the current values stored in a .HEAD file, use the command\n"
        "'3dinfo dataset'.  Using 3dinfo both before and after 3drefit is\n"
        "a good idea to make sure the changes have been made correctly!\n"
        "\n"
        "20 Jun 2006: 3drefit will now work on NIfTI datasets (but it will write\n"
        "             out the entire dataset, into the current working directory)\n\n"
         ) ;

   printf(
"Usage: 3drefit [options] dataset ... ~1~\n"
"where the options are\n"
"  -quiet          Turn off the verbose progress messages\n"
"\n"
"  -orient code    Sets the orientation of the 3D volume(s) in the .BRIK.\n"
"                  The code must be 3 letters, one each from the\n"
"                  pairs {R,L} {A,P} {I,S}.  The first letter gives\n"
"                  the orientation of the x-axis, the second the\n"
"                  orientation of the y-axis, the third the z-axis:\n"
"                     R = right-to-left         L = left-to-right\n"
"                     A = anterior-to-posterior P = posterior-to-anterior\n"
"                     I = inferior-to-superior  S = superior-to-inferior\n"
"               ** WARNING: when changing the orientation, you must be sure\n"
"                  to check the origins as well, to make sure that the volume\n"
"                  is positioned correctly in space.\n"
"\n"
"  -xorigin distx  Puts the center of the edge voxel off at the given\n"
"  -yorigin disty  distance, for the given axis (x,y,z); distances in mm.\n"
"  -zorigin distz  (x=first axis, y=second axis, z=third axis).\n"
"                  Usually, only -zorigin makes sense.  Note that this\n"
"                  distance is in the direction given by the corresponding\n"
"                  letter in the -orient code.  For example, '-orient RAI'\n"
"                  would mean that '-zorigin 30' sets the center of the\n"
"                  first slice at 30 mm Inferior.  See the to3d manual\n"
"                  for more explanations of axes origins.\n"
"               ** SPECIAL CASE: you can use the string 'cen' in place of\n"
"                  a distance to force that axis to be re-centered.\n"
"\n"
"  -xorigin_raw xx Puts the center of the edge voxel at the given COORDINATE\n"
"  -yorigin_raw yy rather than the given DISTANCE.  That is, these values\n"
"  -zorigin_raw zz directly replace the offsets in the dataset header,\n"
"                  without any possible sign changes.\n"
"\n"
"  -duporigin cset Copies the xorigin, yorigin, and zorigin values from\n"
"                  the header of dataset 'cset'.\n"
"\n"
"  -dxorigin dx    Adds distance 'dx' (or 'dy', or 'dz') to the center\n"
"  -dyorigin dy    coordinate of the edge voxel.  Can be used with the\n"
"  -dzorigin dz    values input to the 'Nudge xyz' plugin.\n"
"               ** WARNING: you can't use these options at the same\n"
"                  time you use -orient.\n"
"               ** WARNING: consider -shift_tags if dataset has tags\n"
"\n"
"  -xdel dimx      Makes the size of the voxel the given dimension,\n"
"  -ydel dimy      for the given axis (x,y,z); dimensions in mm.\n"
"  -zdel dimz   ** WARNING: if you change a voxel dimension, you will\n"
"                  probably have to change the origin as well.\n"
"  -keepcen        When changing a voxel dimension with -xdel (etc.),\n"
"                  also change the corresponding origin to keep the\n"
"                  center of the dataset at the same coordinate location.\n"
"  -xyzscale fac   Scale the size of the dataset voxels by the factor 'fac'.\n"
"                  This is equivalent to using -xdel, -ydel, -zdel together.\n"
"                  -keepcen is used on the first input dataset, and then\n"
"                  any others will be shifted the same amount, to maintain\n"
"                  their alignment with the first one.\n"
"               ** WARNING: -xyzscale can't be used with any of the other\n"
"                  options that change the dataset grid coordinates!\n"
"               ** N.B.: 'fac' must be positive, and using fac=1.0 is stupid.\n"
"\n"
"  -TR time        Changes the TR time to a new value (see 'to3d -help').\n"
"               ** You can also put the name of a dataset in for 'time', in\n"
"                  which case the TR for that dataset will be used.\n"
"               ** N.B.: If the dataset has slice time offsets, these will\n"
"                  be scaled by the factor newTR/oldTR. This scaling does not\n"
"                  apply if you use '-Tslices' in the same 3drefit run.\n"
"  -notoff         Removes the slice-dependent time-offsets.\n"
"  -Torg ttt       Set the time origin of the dataset to value 'ttt'.\n"
"                  (Time origins are set to 0 in to3d.)\n"
"               ** WARNING: These 3 options apply only to 3D+time datasets.\n"
"                   **N.B.: Using '-TR' on a dataset without a time axis\n"
"                           will add a time axis to the dataset.\n"
"\n"
"  -Tslices a b c d ...\n"
"                  Reset the slice time offsets to be 'a', 'b', 'c', ...\n"
"                  (in seconds). The number of values following '-Tslices'\n"
"                  should be the same as the number of slices in the dataset,\n"
"                  but 3drefit does NOT check that this is true.\n"
"               ** If any offset time is < 0 or >= TR, a warning will be\n"
"                  printed (to stderr), but this is not illegal even though\n"
"                  it is a bad idea.\n"
"               ** If the dataset does not have a TR set, then '-Tslices'\n"
"                  will fail. You can use '-TR' to set the inter-volume time\n"
"                  spacing in the same 3drefit command.\n"
"               ** If you have the slices times stored (e.g., from DICOM) in\n"
"                  some other units, you can scale them to be in seconds by\n"
"                  putting a scale factor after the '-Tslices' option as follows:\n"
"                    -Tslices '*0.001' 300 600 900 ...\n"
"                  which would be used to scale from milliseconds to seconds.\n"
"                  The format is to start the scale factor with a '*' to tell\n"
"                  3drefit that this number is not a slice offset but is to be\n"
"                  used a a scale factor for the rest of the following values.\n"
"                  Since '*' is a filename wildcard, it needs to be in quotes!\n"
"               ** The program stops looking for number values after '-Tslices'\n"
"                  when it runs into something that does not look like a number.\n"
"                  Here, 'look like a number' means a character string that:\n"
"                    * starts with a digit 0..9\n"
"                    * starts with a decimal point '.' followed by a digit\n"
"                    * starts with a minus sign '-' followed by a digit\n"
"                    * starts with '-.' followed by a digit\n"
"                  So if the input dataset name starts with a digit, and the\n"
"                  last command line option '-Tslices', 3drefit will think\n"
"                  the filename is actually a number for a slice offset time.\n"
"                  To avoid this problem, you can do one of these things:\n"
"                    * Put in an option that is just the single character '-'\n"
"                    * Don't use '-Tslices' as the last option\n"
"                    * Put a directory name before the dataset name, as in\n"
"                      './Galacticon.nii'\n"
"                ** If you have the slice time offsets stored in a text file\n"
"                   as a list of values, then you can input these values on\n"
"                   the command line using the Unix backquote operator, as in\n"
"                     -Tslices `cat SliceTimes.1D`\n"
"\n"
"  -newid          Changes the ID code of this dataset as well.\n"
"\n"
"  -nowarp         Removes all warping information from dataset.\n"
"\n"
"  -apar aset      Set the dataset's anatomy parent dataset to 'aset'\n"
"               ** N.B.: The anatomy parent is the dataset from which the\n"
"                  transformation from +orig to +acpc and +tlrc coordinates\n"
"                  is taken.  It is appropriate to use -apar when there is\n"
"                  more than 1 anatomical dataset in a directory that has\n"
"                  been transformed.  In this way, you can be sure that\n"
"                  AFNI will choose the correct transformation.  You would\n"
"                  use this option on all the +orig dataset that are\n"
"                  aligned with 'aset' (i.e., that were acquired in the\n"
"                  same scanning session).\n"
"               ** N.B.: Special cases of 'aset'\n"
"                   aset = NULL --> remove the anat parent info from the dataset\n"
"                   aset = SELF --> set the anat parent to be the dataset itself\n"
"\n"
"  -wpar wset      Set the warp parent (the +orig version of a +tlrc dset).\n"
"                  This option is used by @auto_tlrc. Do not use it unless\n"
"                  you know what you're doing. \n"
"\n"
"  -clear_bstat    Clears the statistics (min and max) stored for each sub-brick\n"
"                  in the dataset.  This is useful if you have done something to\n"
"                  modify the contents of the .BRIK file associated with this\n"
"                  dataset.\n"
"  -redo_bstat     Re-computes the statistics for each sub-brick.  Requires\n"
"                  reading the .BRIK file, of course.  Also does -clear_bstat\n"
"                  before recomputing statistics, so that if the .BRIK read\n"
"                  fails for some reason, then you'll be left without stats.\n"
"\n"
"  -statpar v ...  Changes the statistical parameters stored in this\n"
"                  dataset.  See 'to3d -help' for more details.\n"
"\n"
"  -markers        Adds an empty set of AC-PC markers to the dataset,\n"
"                  if it can handle them (is anatomical, is in the +orig\n"
"                  view, and isn't 3D+time).\n"
"               ** WARNING: this will erase any markers that already exist!\n"
"\n"
"  -shift_tags     Apply -dxorigin (and y and z) changes to tags.\n"
"\n"
"  -dxtag dx       Add dx to the coordinates of all tags.\n"
"  -dytag dy       Add dy to the coordinates of all tags.\n"
"  -dztag dz       Add dz to the coordinates of all tags.\n"
"\n"
"  -view code      Changes the 'view' to be 'code', where the string 'code'\n"
"                  is one of 'orig', 'acpc', or 'tlrc'.\n"
"               ** WARNING: The program will also change the .HEAD and .BRIK\n"
"                  filenames to match.  If the dataset filenames already\n"
"                  exist in the '+code' view, then this option will fail.\n"
"                  You will have to rename the dataset files before trying\n"
"                  to use '-view'.  If you COPY the files and then use\n"
"                  '-view', don't forget to use '-newid' as well!\n"
"               ** WARNING2: Changing the view without specifying the new \n"
"                  might lead to conflicting information. Consider specifying\n"
"                  the space along with -view\n"
"  -space spcname  Associates the dataset with a specific template type, e.g.\n"
"                  TLRC, MNI, ORIG. The default assumed for +tlrc datasets is\n"
"                  'TLRC'. One use for this attribute is to use MNI space\n"
"                  coordinates and atlases instead of the default TLRC space.\n"
"               ** See WARNING2 for -view option.\n"
"  -cmap cmaptype  Associate colormap type with dataset. Available choices are\n"
"                  CONT_CMAP (the default), INT_CMAP (integer colormap display)\n"
"                  and SPARSE_CMAP (for sparse integer colormaps). INT_CMAP is\n"
"                  appropriate for showing ROI mask datasets or Atlas datasets\n"
"                  where the continuous color scales are not useful.\n"
"\n"
"  -label2 llll    Set the 'label2' field in a dataset .HEAD file to the\n"
"                  string 'llll'.  (Can be used as in AFNI window titlebars.)\n"
"  -labeltable TTT Inset the label table TTT in the .HEAD file.\n"
"                  The label table format is described in README.environment\n"
"                  under the heading: 'Variable: AFNI_VALUE_LABEL_DTABLE'\n"
"              See also -copytables\n"
"\n"
"  -denote         Means to remove all possibly-identifying notes from\n"
"                  the header.  This includes the History Note, other text\n"
"                  Notes, keywords, and labels.\n"
"\n"
"  -deoblique      Replace transformation matrix in header with cardinal matrix.\n"
"                  This option DOES NOT deoblique the volume. To do so\n"
"                  you should use 3dWarp -deoblique. This option is not \n"
"                  to be used unless you really know what you're doing.\n\n"
"  -oblique_origin\n"
"                  assume origin and orientation from oblique transformation\n"
"                  matrix rather than traditional cardinal information\n\n"

"  -byteorder bbb  Sets the byte order string in the header.\n"
"                  Allowable values for 'bbb' are:\n"
"                     LSB_FIRST   MSB_FIRST   NATIVE_ORDER\n"
"                  Note that this does not change the .BRIK file!\n"
"                  This is done by programs 2swap and 4swap.\n"
"\n"
"  -checkaxes      Doesn't alter the input dataset; rather, this just\n"
"                  checks the dataset axes orientation codes and the\n"
"                  axes matrices for consistency.  (This option was\n"
"                  added primarily to check for bugs in various codes.)\n"
"\n"
"  -appkey ll      Appends the string 'll' to the keyword list for the\n"
"                  whole dataset.\n"
"  -repkey ll      Replaces the keyword list for the dataset with the\n"
"                  string 'll'.\n"
"  -empkey         Destroys the keyword list for the dataset.\n"
   ) ;

   printf(
    "\n"
    "  -atrcopy dd nn  Copy AFNI header attribute named 'nn' from dataset 'dd'\n"
    "                  into the header of the dataset(s) being modified.\n"
    "                  For more information on AFNI header attributes, see\n"
    "                  documentation file README.attributes. More than one\n"
    "                  '-atrcopy' option can be used.\n"
    "          **N.B.: This option is for those who know what they are doing!\n"
    "                  Without the -saveatr option, this option is\n"
    "                  meant to be used to alter attributes that are NOT\n"
    "                  directly mapped into dataset internal structures, since\n"
    "                  those structures are mapped back into attribute values\n"
    "                  as the dataset is being written to disk.  If you want\n"
    "                  to change such an attribute, you have to use the\n"
    "                  corresponding 3drefit option directly or use the \n"
    "                  -saveatr option.\n"
    "\n"
    "                  If you are confused, try to understand this: \n"
    "                  Option -atrcopy was never intended to modify AFNI-\n"
    "                  specific attributes. Rather, it was meant to copy\n"
    "                  user-specific attributes that had been added to some\n"
    "                  dataset using -atrstring option. A cursed day came when\n"
    "                  it was convenient to use -atrcopy to copy an AFNI-specific\n"
    "                  attribute (BRICK_LABS to be exact) and for that to\n"
    "                  take effect in the output, the option -saveatr was added.\n"
    "                  Contact Daniel Glen and/or Rick Reynolds for further \n"
    "                  clarification and any other needs you may have.\n"
    "\n"
    "                  Do NOT use -atrcopy or -atrstring with other modification\n"
    "                  options.\n"
    "          See also -copyaux\n"
    "\n"
    "  -atrstring n 'x' Copy the string 'x' into the dataset(s) being\n"
    "                   modified, giving it the attribute name 'n'.\n"
    "                   To be safe, the 'x' string should be in quotes.\n"
    "          **N.B.: You can store attributes with almost any name in\n"
    "                  the .HEAD file.  AFNI will ignore those it doesn't\n"
    "                  know anything about.  This technique can be a way of\n"
    "                  communicating information between programs.  However,\n"
    "                  when most AFNI programs write a new dataset, they will\n"
    "                  not preserve any such non-standard attributes.\n"
    "          **N.B.: Special case: if the string 'x' is of the form\n"
    "                  'file:name', then the contents of the file 'name' will\n"
    "                  be read in as a single string and stored in the attribute.\n"
    "  -atrfloat name 'values'\n"
    "  -atrint name 'values'\n"
    "                  Create or modify floating point or integer attributes.\n"
    "                  The input values may be specified as a single string\n"
    "                  in quotes or as a 1D filename or string. For example,\n"
    "     3drefit -atrfloat IJK_TO_DICOM_REAL '1 0.2 0 0 -0.2 1 0 0 0 0 1 0'"
          " dset+orig\n"
    "     3drefit -atrfloat IJK_TO_DICOM_REAL flipZ.1D dset+orig\n"
    "     3drefit -atrfloat IJK_TO_DICOM_REAL \\ \n"
    "       '1D:1,0.2,2@0,-0.2,1,2@0,2@0,1,0' \\ \n"
    "       dset+orig\n"
    "                  Almost all afni attributes can be modified in this way\n"
    "  -saveatr        (default) Copy the attributes that are known to AFNI into \n"
    "                  the dset->dblk structure thereby forcing changes to known\n"
    "                  attributes to be present in the output.\n"
    "                  This option only makes sense with -atrcopy\n"
    "          **N.B.: Don't do something like copy labels of a dataset with \n"
    "                  30 sub-bricks to one that has only 10, or vice versa.\n"
    "                  This option is for those who would deservedly earn a\n"
    "                  hunting license.\n"
    "  -nosaveatr      Opposite of -saveatr\n"
    "     Example: \n"
    "     3drefit -saveatr -atrcopy WithLabels+tlrc BRICK_LABS NeedsLabels+tlrc\n"
   ) ;

   printf(
    "\n"
    "  -'type'         Changes the type of data that is declared for this\n"
    "                  dataset, where 'type' is chosen from the following:\n"
   ) ;
   printf("       ANATOMICAL TYPES\n") ;
   for( ii=FIRST_ANAT_TYPE ; ii <= LAST_ANAT_TYPE ; ii++ ){
      printf("     %8s == %-16.16s",ANAT_prefixstr[ii],ANAT_typestr[ii] ) ;
      if( (ii-FIRST_ANAT_TYPE)%2 == 1 ) printf("\n") ;
   }
   if( (ii-FIRST_ANAT_TYPE)%2 == 1 ) printf("\n") ;
   printf("       FUNCTIONAL TYPES\n") ;
   for( ii=FIRST_FUNC_TYPE ; ii <= LAST_FUNC_TYPE ; ii++ ){
      printf("     %8s == %-16.16s",FUNC_prefixstr[ii],FUNC_typestr[ii] ) ;
      if( (ii-FIRST_ANAT_TYPE)%2 == 1 ) printf("\n") ;
   }
   if( (ii-FIRST_ANAT_TYPE)%2 == 1 ) printf("\n") ;

   printf(           /* 08 Jun 2004 */
    "\n"
    "  -copyaux auxset Copies the 'auxiliary' data from dataset 'auxset'\n"
    "                  over the auxiliary data for the dataset being\n"
    "                  modified.  Auxiliary data comprises sub-brick labels,\n"
    "                  keywords, statistics codes, nodelists, and labeltables\n"
    "                  AND/OR atlas point lists.\n"
    "                  '-copyaux' occurs BEFORE the '-sub' operations below,\n"
    "                  so you can use those to alter the auxiliary data\n"
    "                  that is copied from auxset.\n"
    "\n" ) ;

   printf(           /* 11 Jan 2012 */
    "\n"
    "  -copytables tabset Copies labeltables AND/OR atlas point lists, if any,\n"
    "                  from tabset to the input dataset.\n"
    "                  '-copyaux' occurs BEFORE the '-sub' operations below,\n"
    "                  so you can use those to alter the auxiliary data\n"
    "                  that is copied from tabset. \n"
    "\n" ) ;


   printf(
    "  -relabel_all xx  Reads the file 'xx', breaks it into strings,\n"
    "                   and puts these strings in as the sub-brick\n"
    "                   labels.  Basically a batch way of doing\n"
    "                   '-sublabel' many times, for n=0, 1, ...\n"
    "                 ** This option is executed BEFORE '-sublabel',\n"
    "                    so any labels from '-sublabel' will over-ride\n"
    "                    labels from this file.\n"
    "                 ** Strings in the 'xx' file are separated by\n"
    "                    whitespace (blanks, tabs, new lines).\n"
    "\n" ) ;

   printf(
    "  -relabel_all_str 'lab0 lab1 ... lab_p': Just like -relabel_all\n"
    "                   but with labels all present in one string\n"
    "\n" ) ;

   printf(
    "  -sublabel_prefix PP: Prefix each sub-brick's label with PP\n"
    "  -sublabel_suffix SS: Suffix each sub-brick's label with SS\n"
    "\n" ) ;

   printf(
    "The options below attach auxiliary data to sub-bricks in the dataset. ~1~\n"
    "Each option may be used more than once so that\n"
    "multiple sub-bricks can be modified in a single run of 3drefit.\n"
    "\n"
    "  -sublabel  n ll  Attach to sub-brick #n the label string 'll'.\n"
    "  -subappkey n ll  Add to sub-brick #n the keyword string 'll'.\n"
    "  -subrepkey n ll  Replace sub-brick #n's keyword string with 'll'.\n"
    "  -subempkey n     Empty out sub-brick #n' keyword string\n"
    "\n"
    "  -substatpar n type v ...\n"
    "                  Attach to sub-brick #n the statistical type and\n"
    "                  the auxiliary parameters given by values 'v ...',\n"
    "                  where 'type' is one of the following:\n"
   ) ;
   printf("         Stat Types: ~2~\n"
          "         type  Description  PARAMETERS\n"
          "         ----  -----------  ----------------------------------------\n" ) ;
   for( ii=FIRST_FUNC_TYPE ; ii <= LAST_FUNC_TYPE ; ii++ ){
     if( FUNC_IS_STAT(ii) )
       printf("         %4s  %-11.11s  %s\n",
              FUNC_prefixstr[ii] , FUNC_typestr[ii]+6 , FUNC_label_stat_aux[ii] ) ;
   }
   printf(
     "\n"
     "You can also use option '-unSTAT' to remove all statistical encodings\n"
     "from sub-bricks in the dataset.  This operation would be desirable if\n"
     "you modified the values in the dataset (e.g., via 3dcalc).\n"
     " ['-unSTAT' is done BEFORE the '-substatpar' operations, so you can  ]\n"
     " [combine these options to completely redo the sub-bricks, if needed.]\n"
     " [Option '-unSTAT' also implies that '-unFDR' will be carried out.   ]\n"
   ) ;

   printf(
    "\n"
    "The following options allow you to modify VOLREG fields: ~1~\n"
    "  -vr_mat val1 ... val12  Use these twelve values for VOLREG_MATVEC_index.\n"
    "  -vr_mat_ind index       Index of VOLREG_MATVEC_index field to be modified.\n"
    "                          Optional, default index is 0.\n"
    "NB: You can only modify one VOLREG_MATVEC_index at a time\n"
    "  -vr_center_old x y z    Use these 3 values for VOLREG_CENTER_OLD.\n"
    "  -vr_center_base x y z   Use these 3 values for VOLREG_CENTER_BASE.\n"
    "\n"
   );

   printf(
    "\n"
    "The following options let you modify the FDR curves stored in the header: ~1~\n"
    "\n"
    " -addFDR = For each sub-brick marked with a statistical code, (re)compute\n"
    "           the FDR curve of z(q) vs. statistic, and store in the dataset header\n"
    "           * '-addFDR' runs as if '-new -pmask' were given to 3dFDR, so that\n"
    "              stat values == 0 will be ignored in the FDR algorithm.\n"
    "\n"
    " -FDRmask mset = load dataset 'mset' and use it as a mask\n"
    " -STATmask mset  for the '-addFDR' calculations.\n"
    "                 * This can be useful if you ran 3dDeconvolve/3dREMLFIT\n"
    "                    without a mask, and want to apply a mask to improve\n"
    "                    the FDR estimation procedure.\n"
    "                 * If '-addFDR' is NOT given, then '-FDRmask' does nothing.\n"
    "                 * 3drefit does not generate an automask for FDR purposes\n"
    "                    (unlike 3dREMLfit and 3dDeconvolve), since the input\n"
    "                    dataset may contain only statistics and no structural\n"
    "                    information about the brain.\n"
    "\n"
    " -unFDR  = Remove all FDR curves from the header\n"
    "           [you will want to do this if you have done something to ]\n"
    "           [modify the values in the dataset statistical sub-bricks]\n"
    "\n"
   ) ;

   printf("++ Last program update: 27 Mar 2009\n");

   PRINT_COMPILE_DATE ; exit(0) ;
}

void SynErr(char *str)
{
   if (str) ERROR_exit(str) ;  /* does not return */
   else Syntax(0);
   return;
}


#define ASET_NULL 1
#define ASET_SELF 2

int main( int argc , char *argv[] )
{
   THD_3dim_dataset * dset = NULL, * aset = NULL , *waset = NULL;
                      int aset_code = 0    ; /* 14 Dec 1999 */
                      int waset_code = 0;
   THD_dataxes      * daxes = NULL;
   int new_stuff = 0 ;
   int new_orient = 0 ; char orient_code[4]={"LOV"} ; int xxor=0,yyor=0,zzor=0 ;
   int new_xorg   = 0 ; float xorg = 0.0; int cxorg=0, dxorg=0 , duporg=0 ;
   int new_yorg   = 0 ; float yorg = 0.0; int cyorg=0, dyorg=0 ;
   int new_zorg   = 0 ; float zorg = 0.0; int czorg=0, dzorg=0 ;
   int new_tags   = 0 ; int shift_tags = 0 ; /* 08 May 2006 [rickr] */
                        float dxtag=0.0, dytag=0.0, dztag=0.0 ;
   int new_xdel   = 0 ; float xdel = 0.0;
   int new_ydel   = 0 ; float ydel = 0.0;
   int new_zdel   = 0 ; float zdel = 0.0;
   int new_TR     = 0 ; float TR = 0.0;
   int new_Tslices= 0 ; float *Tslices = NULL ; /* 18 Dec 2018 */
   int new_Torg   = 0 ; float Torg = 0.0; /* 29 Jan 2003 */
   int new_tunits = 0 ; int tunits = 0;
   int new_idcode = 0 ;
   int new_nowarp = 0 ;
   int new_stataux= 0 ; float stataux[MAX_STAT_AUX] ;
   int new_type   = 0 ; int dtype = 0 , ftype = 0, nvals = 0;
   int new_markers= 0 ;
   int new_view   = 0 ; int vtype = 0;
   int new_key    = 0 ; char * key = NULL;
   int new_byte_order = 0 ;          /* 25 Apr 1998 */
   int new_toff_sl    = 0 ;          /* 12 Feb 2001 */
   int clear_bstat    = 0 ;          /* 28 May 2002 */
   int redo_bstat     = 0 ;          /* 01 Feb 2005 */
   int copyaux        = 0 ;          /* 08 Jun 2004 */
   int copytabs       = 0 ;          /* 11 Jan 2012 */
   THD_3dim_dataset *auxset=NULL ;   /* 08 Jun 2004 */
   THD_3dim_dataset *tabset=NULL ;   /* 11 Jan 2012 */
   char *new_label2   = NULL ;       /* 21 Dec 2004 */
   char *labeltable   = NULL;        /* 25 Feb 2010 ZSS */
   int denote         = 0 ;          /* 08 Jul 2005 */
   Boolean write_output ;            /* 20 Jun 2006 [rickr] */
   int keepcen        = 0 ;          /* 17 Jul 2006 [RWCox] */
   float xyzscale     = 0.0f ;       /* 17 Jul 2006 */
   int deoblique  = 0;               /* 20 Jun 2007 [drg] */
   int use_oblique_origin = 0;       /* 01 Dec 2008 */
   int do_FDR = 0 ;                  /* 23 Jan 2008 [RWCox] */
   int do_killSTAT = 0 ;             /* 24 Jan 2008 [RWCox] */
   int space          = 0 ;          /* 16 Mar 2009 [drg]*/
   char *spacename = NULL;
   byte *FDRmask = NULL ;            /* 27 Mar 2009 [RWcox] */
   int  nFDRmask = 0 ;
   int   ndone=0 ;                   /* 18 Jul 2006 */
   int   verb =1 ;
   int   did_something ;             /* 30 Mar 2010 */
   int cmap = -1;                    /* colormap handling */
   NI_str_array *sar_relab=NULL ;    /* 18 Apr 2011 */
   int geom_change = 0;              /* 04 Nov 2011 [drg] */
   int do_checkaxes = 0 ;            /* 27 Jun 2014 [RWCox] */

#define VINFO(x) do{ if(verb)ININFO_message(x) ; } while(0)

   char str[256] ;
   int  iarg , ii ;

   typedef struct { int iv ; char lab[THD_MAX_SBLABEL] ; }     SUBlabel   ;
   typedef struct { int iv ; float par[MAX_STAT_AUX+2] ; } SUBstatpar ;
   typedef struct { int iv , code ; char * keyword ; }     SUBkeyword ;
   int nsublab     = 0 ; SUBlabel *   sublab     = NULL ;
   int nsubstatpar = 0 ; SUBstatpar * substatpar = NULL ;
   int nsubkeyword = 0 ; SUBkeyword * subkeyword = NULL ;
   char *cpt ;
   int iv ;

   float volreg_mat[12];
   float center_old[3];
   float center_base[3];
   int Do_volreg_mat = 0, Do_center_old = 0, Do_center_base = 0,
       volreg_matind = 0, icnt = 0;
   char *lcpt=NULL, *subsuff=NULL, *subpref=NULL;

   int   num_atrcopy = 0 ;    /* 03 Aug 2005 */
   ATR_any **atrcopy = NULL ;
   ATR_float  *atr_flt ;

   int saveatr = 1;
   int atrmod = 0;  /* if no ATR is modified, don't overwrite normal changes */
                                                      /* 28 Jul 2006 [rickr] */
   THD_dmat33 tmat ;
   THD_dfvec3 tvec ;

   int code, acount;

   /*-------------------------- help me if you can? --------------------------*/

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3drefit main"); machdep() ; PRINT_VERSION("3drefit") ; AUTHOR("RW Cox") ;
   set_obliquity_report(0); /* silence obliquity */
   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3drefit",argc,argv) ;
   AFNI_setenv("AFNI_COMPRESSOR=NONE") ; /* 26 Jul 2013 */

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){
      if(strcmp(argv[iarg],"-h") == 0 ||
         strncmp(argv[iarg],"-help",4) == 0 )
         Syntax(strlen(argv[iarg]) > 3 ? 2:1) ;

      if( strcmp(argv[iarg],"-") == 0 ){ iarg++ ; continue ; } /* 18 Dec 2018 */

      /*----- -checkaxes [27 Jun 2014] -----*/

      if( strcasecmp(argv[iarg],"-checkaxes") == 0 ){
        do_checkaxes = 1 ;  new_stuff++ ; iarg++ ; continue ;
      }

      /*----- -addFDR [23 Jan 2008] -----*/

      if( strcasecmp(argv[iarg],"-addFDR") == 0 ){
        do_FDR = 1 ;  new_stuff++ ; iarg++ ; continue ;
      }
      if( strcasecmp(argv[iarg],"-killFDR") == 0 || strcasecmp(argv[iarg],"-unFDR") == 0 ){
        do_FDR = -1 ;  new_stuff++ ; iarg++ ; continue ;
      }
      if( strcasecmp(argv[iarg],"-killSTAT") == 0 || strcasecmp(argv[iarg],"-unSTAT") == 0 ){
        do_killSTAT = 1 ; do_FDR = -1 ; new_stuff++ ; iarg++ ; continue ;
      }

      if( strcasecmp(argv[iarg],"-FDRmask")  == 0 ||
          strcasecmp(argv[iarg],"-STATmask") == 0   ){   /*-- 27 Mar 2009 --*/

        bytevec *bvec ;  /* 15 Jul 2010 */

        if( iarg+1 >= argc ) SynErr("need 1 argument after -FDRmask!") ;
        if( nFDRmask > 0 )   SynErr("can't have two -FDRmask options!") ;
        bvec = THD_create_mask_from_string(argv[++iarg]) ;
        if( bvec == NULL ) ERROR_exit("Can't decipher %s",argv[iarg-1]) ;
        FDRmask = bvec->ar ; nFDRmask = bvec->nar ;
        ii = THD_countmask(nFDRmask,FDRmask) ;
        if( ii < 100 ){
          WARNING_message("-FDRmask has only %d nonzero voxels: ignoring",ii) ;
          KILL_bytevec(bvec) ; FDRmask = NULL ; nFDRmask = 0 ;
        } else {
          INFO_message("%s has %d nonzero voxels (out of %d total)",
                       argv[iarg-1],ii,nFDRmask) ;
        }
        iarg++ ; continue ;
      }

      /*----- -atrcopy dd nn [03 Aug 2005] -----*/

      if( strcmp(argv[iarg],"-atrcopy") == 0 ){
        THD_3dim_dataset *qset ; ATR_any *atr ;

        if( iarg+2 >= argc ) SynErr("need 2 arguments after -atrcopy!") ;

        qset = THD_open_dataset( argv[++iarg] ) ;
        if( qset == NULL ){
          WARNING_message("Can't open -atrcopy dataset %s",argv[iarg]) ;
          iarg++ ; goto atrcopy_done ;
        }
        atr = THD_find_atr( qset->dblk , argv[++iarg] ) ;
        if( atr == NULL ){
          WARNING_message("Can't find attribute %s in -atrcopy dataset %s",
                          argv[iarg],argv[iarg-1]) ;
          DSET_delete(qset) ; goto atrcopy_done ;
        }

        atrcopy = (ATR_any **)realloc( (void *)atrcopy ,
                                       sizeof(ATR_any *)*(num_atrcopy+1) ) ;
        atrcopy[num_atrcopy++] = THD_copy_atr( atr ) ;
        /* atr_print( atr, NULL , NULL, '\0', 1) ;  */
        DSET_delete(qset) ;
        atrmod = 1;  /* replaced new_stuff   28 Jul 2006 rcr */

      atrcopy_done:
        iarg++ ; continue ;
      }

      /*----- -atrstring nn xx [03 Aug 2005] -----*/

      if( strcmp(argv[iarg],"-atrstring") == 0 ){
        ATR_string *atr ; char *aname , *xx , *yy=NULL ;

        if( iarg+2 >= argc ) SynErr("need 2 arguments after -atrstring!") ;

        aname = argv[++iarg] ;
        if( !THD_filename_pure(aname) ){
          WARNING_message("Illegal -atrstring name %s",aname) ;
          iarg++ ; goto atrstring_done ;
        }
        xx  = argv[++iarg] ;
        atr = (ATR_string *)XtMalloc(sizeof(ATR_string)) ;

        if( strncmp(xx,"file:",5) == 0 && strlen(xx) > 5 ){  /* 08 Jul 2010 */
          int ii ;
          yy = AFNI_suck_file(xx+5) ;
          if( yy == NULL ){
            WARNING_message("Can't read '%s'",xx) ; goto atrstring_done ;
          }
          xx = yy ;
          for( ii=strlen(yy)-1 ; ii > 0 && isspace(yy[ii]) ; ii-- )
            yy[ii] = '\0' ;   /* truncate trailing whitespace */
        }

        atr->type = ATR_STRING_TYPE ;
        atr->name = XtNewString( aname ) ;
        atr->nch  = strlen(xx)+1 ; ;
        atr->ch   = (char *)XtMalloc( sizeof(char) * atr->nch ) ;
        memcpy( atr->ch , xx , sizeof(char) * atr->nch ) ;

        if( yy != NULL ) free(yy) ;  /* 08 Jul 2010 */

        atrcopy = (ATR_any **)realloc( (void *)atrcopy ,
                                       sizeof(ATR_any *)*(num_atrcopy+1) ) ;
        atrcopy[num_atrcopy++] = (ATR_any *)atr ;

        atrmod = 1;  /* replaced new_stuff++   28 Jul 2006 [rickr] */
       atrstring_done:
        iarg++ ; continue ;
      }


      /*----- -atrfloat name "xx.xx yy.yy ..." [02 Oct 2008] -----*/
      if( strcmp(argv[iarg],"-atrfloat") == 0 ){
        ATR_float *atr ;

        if( iarg+2 >= argc ) SynErr("need 2 arguments after -atrfloat!") ;
        atr = Update_float_atr(argv[iarg+1], argv[iarg+2]);
        if(atr) {
           /* add this float attribute to list of attributes being modified */
           atrcopy = (ATR_any **)realloc( (void *)atrcopy ,
                                       sizeof(ATR_any *)*(num_atrcopy+1) ) ;
           atrcopy[num_atrcopy++] = (ATR_any *)atr ;

           atrmod = 1;  /* replaced new_stuff++   28 Jul 2006 [rickr] */
        }

        iarg+=3 ; continue ;
      }

      /*----- -atrint name "xx.xx yy.yy ..." [06 Oct 2008] -----*/
      if( strcmp(argv[iarg],"-atrint") == 0 ){
        ATR_int *atr ;

        if( iarg+2 >= argc ) SynErr("need 2 arguments after -atrint!") ;
        atr = Update_int_atr(argv[iarg+1], argv[iarg+2]);
        if(atr) {
           /* add this int attribute to list of attributes being modified */
           atrcopy = (ATR_any **)realloc( (void *)atrcopy ,
                                       sizeof(ATR_any *)*(num_atrcopy+1) ) ;
           atrcopy[num_atrcopy++] = (ATR_any *)atr ;

           atrmod = 1;  /* new or modified attribute */
        }

        iarg+=3 ; continue ;
      }


      if( strcmp(argv[iarg],"-saveatr") == 0 ){
        saveatr = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-nosaveatr") == 0 ){
        saveatr = 0 ; iarg++ ; continue ;
      }

      /*----- -denote [08 Jul 2005] -----*/

      if( strcmp(argv[iarg],"-denote") == 0 ){
        denote = 1 ; new_stuff++ ; iarg++ ; continue ;
      }

      /*----- -copyaux auxset [08 Jun 2004] -----*/

      if( strcmp(argv[iarg],"-copyaux") == 0 ){

         if( iarg+1 >= argc ) SynErr("need 1 argument after -copyaux!") ;

         if( auxset != NULL ) SynErr("Can't have more than one -copyaux option!") ;

         iarg++ ; copyaux = 1 ;
         if( strcmp(argv[iarg],"NULL") == 0 ){  /* special case */
            auxset = NULL ;
         } else {
            auxset = THD_open_one_dataset( argv[iarg] ) ;
            if( auxset == NULL ) SynErr("Can't open -copyaux dataset!") ;
         }

         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -copytables tabset [12 Jan 2012] -----*/

      if( strcmp(argv[iarg],"-copytables") == 0 ){

         if( iarg+1 >= argc ) SynErr("need 1 argument after -copytables!") ;

         if( tabset != NULL ) SynErr("Can't have more than one -copytables option!") ;

         iarg++ ; copytabs = 1 ;

         tabset = THD_open_one_dataset( argv[iarg] ) ;
         if( tabset == NULL ) SynErr("Can't open -copytables dataset!") ;

         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -apar aset [14 Oct 1999] -----*/

      if( strcmp(argv[iarg],"-apar")       == 0 ||
          strcmp(argv[iarg],"-anatparent") == 0 ||
          strcmp(argv[iarg],"-aset")       == 0    ){

         if( iarg+1 >= argc )
            SynErr("need 1 argument after -apar!") ;

         if( aset != NULL || aset_code != 0 )                 /* 13-14 Dec 1999 */
            SynErr("Can't have more than one -apar option!");

         iarg++ ;
         if( strcmp(argv[iarg],"NULL") == 0 ){    /* 14 Dec 1999: special cases */
            aset_code = ASET_NULL ;
         } else if( strcmp(argv[iarg],"SELF") == 0 ){
            aset_code = ASET_SELF ;
         } else {
            aset = THD_open_one_dataset( argv[iarg] ) ;
            if( aset == NULL )
               SynErr("Can't open -apar dataset!") ;
         }

         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -wpar wset [ZSS June 06] -----*/

      if( strcmp(argv[iarg],"-wpar")       == 0 ||
          strcmp(argv[iarg],"-warpparent") == 0 ||
          strcmp(argv[iarg],"-wset")       == 0    ){

         if( iarg+1 >= argc )
            SynErr("need 1 argument after -wpar!") ;

         if( waset != NULL || waset_code != 0 )
            SynErr("Can't have more than one -wpar option!");

         iarg++ ;
         if( strcmp(argv[iarg],"NULL") == 0 ){
            waset_code = ASET_NULL ;
         } else if( strcmp(argv[iarg],"SELF") == 0 ){
            waset_code = ASET_SELF ;
         } else {
            waset = THD_open_one_dataset( argv[iarg] ) ;
            if( waset == NULL )
               SynErr("Can't open -wpar dataset!") ;
         }

         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -clear_bstat option [28 May 2002] -----*/

      if( strcmp(argv[iarg],"-clear_bstat") == 0 ){
         clear_bstat = 1 ;
         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-redo_bstat") == 0 ){  /* 01 Feb 2005 */
         clear_bstat = 1 ; redo_bstat = 1 ;
         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -byteorder option [25 April 1998] -----*/

      if( strncmp(argv[iarg],"-byteorder",7) == 0 ){
         if( iarg+1 >= argc )
            SynErr("need 1 argument after -byteorder!") ;

         iarg++ ;
         if( strcmp(argv[iarg],LSB_FIRST_STRING) == 0 )
            new_byte_order = LSB_FIRST ;
         else if( strcmp(argv[iarg],MSB_FIRST_STRING) == 0 )
            new_byte_order = MSB_FIRST ;
         else if( strcmp(argv[iarg],NATIVE_STRING) == 0 )
            new_byte_order = mri_short_order() ;
         else
            SynErr("illegal argument after -byteorder!") ;

         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -relabel_all option -----*/

      if( strcmp(argv[iarg],"-relabel_all") == 0 ||
          strcmp(argv[iarg],"-relabel_all_str") == 0){   /* 18 Apr 2011 */
        if( ++iarg >= argc ) SynErr("Need argument after -relabel_all*") ;
        if (strcmp(argv[iarg-1],"-relabel_all") == 0 ) {
           char *str ;
           str = AFNI_suck_file(argv[iarg]) ;
           if( str == NULL || *str == '\0' )
             SynErr("Can't read file after -relabel_all") ;
           sar_relab = NI_decode_string_list( str , "`" ) ; free(str) ;
        } else {
           sar_relab = NI_decode_string_list( argv[iarg], "`"  );
        }
        if( sar_relab == NULL || sar_relab->num < 1 )
          SynErr("Can't decode file or string after -relabel_all*") ;
        INFO_message("-relabel_all* %s contains %d label%s" ,
              argv[iarg] , sar_relab->num , (sar_relab->num==1) ? "\0" : "s" ) ;
        new_stuff++ ; iarg++ ; continue ;
      }


      /*----- -sublabel_prefix -----*/

      if( strcmp(argv[iarg],"-sublabel_prefix") == 0 ){   /* 15 Aug 2012 */
        char *str ;
        if( ++iarg >= argc ) SynErr("Need argument after -sublabel_prefix") ;
        subpref = argv[iarg];

        new_stuff++ ; iarg++ ; continue ;
      }

      /*----- -sublabel_suffix -----*/

      if( strcmp(argv[iarg],"-sublabel_suffix") == 0 ){   /* 15 Aug 2012 */
        char *str ;
        if( ++iarg >= argc ) SynErr("Need argument after -sublabel_suffix") ;
        subsuff = argv[iarg] ;

        new_stuff++ ; iarg++ ; continue ;
      }

      /*----- -sublabel option -----*/

      if( strncmp(argv[iarg],"-sublabel",7) == 0 ){
         if( iarg+2 >= argc )
            SynErr("need 2 arguments after -sublabel!") ;

         iv = strtol( argv[++iarg] , &cpt , 10 ) ;
         if( iv < 0 || iv == 0 && cpt == argv[iarg] )
            SynErr("illegal sub-brick index after -sublabel!") ;

         sublab = (SUBlabel *) XtRealloc( (char *)sublab ,
                                          sizeof(SUBlabel) * (nsublab+1) ) ;

         sublab[nsublab].iv = iv ;
         /* max sublabel = 64, 10/28/2011 drg */
         MCW_strncpy( sublab[nsublab].lab , argv[++iarg] , THD_MAX_SBLABEL ) ;
         nsublab++ ; new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -subkeyword options -----*/

      if( strncmp(argv[iarg],"-subappkey",7) == 0 ||
          strncmp(argv[iarg],"-subrepkey",7) == 0 ||
          strncmp(argv[iarg],"-subempkey",7) == 0   ){

         int code , npl ;
              if( strncmp(argv[iarg],"-subappkey",7) == 0 ) code = 1 ;
         else if( strncmp(argv[iarg],"-subrepkey",7) == 0 ) code = 2 ;
         else                                               code = 3 ;

         npl = (code == 3) ? 1 : 2 ;
         if( iarg+npl >= argc )
            SynErr("need arguments after -sub...key!") ;

         iv = strtol( argv[++iarg] , &cpt , 10 ) ;
         if( iv < 0 || iv == 0 && cpt == argv[iarg] )
            SynErr("illegal sub-brick index after -sub...key!") ;

         subkeyword = (SUBkeyword *) XtRealloc( (char *)subkeyword ,
                                                sizeof(SUBkeyword)*(nsubkeyword+1) ) ;

         subkeyword[nsubkeyword].iv   = iv ;
         subkeyword[nsubkeyword].code = code ;
         if( code != 3 ) subkeyword[nsubkeyword].keyword = argv[++iarg] ;

         nsubkeyword++ ; new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -keywords options -----*/

      if( strncmp(argv[iarg],"-appkey",4) == 0 ||
          strncmp(argv[iarg],"-repkey",4) == 0 ||
          strncmp(argv[iarg],"-empkey",4) == 0   ){

         int code , npl ;
              if( strncmp(argv[iarg],"-appkey",4) == 0 ) code = 1 ;
         else if( strncmp(argv[iarg],"-repkey",4) == 0 ) code = 2 ;
         else                                            code = 3 ;

         npl = (code == 3) ? 0 : 1 ;
         if( iarg+code >= argc )
            SynErr("need arguments after -...key!") ;

         new_key = code ;
         if( code != 3 ) key = argv[++iarg] ;
         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -substatpar option -----*/

      if( strncmp(argv[iarg],"-substatpar",7) == 0 ){
         int fc ; float val ;

         if( iarg+2 >= argc )
            SynErr("need at least 2 arguments after -substatpar!") ;

         iv = strtol( argv[++iarg] , &cpt , 10 ) ;
         if( iv < 0 || iv == 0 && cpt == argv[iarg] )
            SynErr("illegal sub-brick index after -substatpar!") ;

         iarg++ ;
         if( strlen(argv[iarg]) < 3 )
            SynErr("illegal type code after -substatpar!") ;
         fc = (argv[iarg][0] == '-') ? 1 : 0 ;

         for( ii=FIRST_FUNC_TYPE ; ii <= LAST_FUNC_TYPE ; ii++ ){
            if( ! FUNC_IS_STAT(ii) ) continue ;
            if( strncmp( &(argv[iarg][fc]) ,
                         FUNC_prefixstr[ii] , THD_MAX_PREFIX ) == 0 ) break ;
         }

         if( ii > LAST_FUNC_TYPE )
            SynErr("unknown type code after -substatpar!") ;

         substatpar = (SUBstatpar *) XtRealloc( (char *)substatpar ,
                                                sizeof(SUBstatpar) * (nsubstatpar+1) ) ;

         substatpar[nsubstatpar].iv     = iv ;
         substatpar[nsubstatpar].par[0] = ii ;
         substatpar[nsubstatpar].par[1] = MAX_STAT_AUX ;

         for( ii=0 ; ii < MAX_STAT_AUX ; ii++ )
            substatpar[nsubstatpar].par[ii+2] = 0.0 ;

         ii = 2 ; iarg++ ;
         do{
            val = strtod( argv[iarg] , &cpt ) ;
            if( *cpt != '\0' ) break ;
            substatpar[nsubstatpar].par[ii++] = val ;
            iarg++ ;
         } while( iarg < argc && ii-2 < MAX_STAT_AUX ) ;

         nsubstatpar++ ; new_stuff++ ; continue ;  /* go to next arg */
      }

      /*----- -orient code option -----*/

#define ORCODE(aa) \
  ( (aa)=='R' ? ORI_R2L_TYPE : (aa)=='L' ? ORI_L2R_TYPE : \
    (aa)=='P' ? ORI_P2A_TYPE : (aa)=='A' ? ORI_A2P_TYPE : \
    (aa)=='I' ? ORI_I2S_TYPE : (aa)=='S' ? ORI_S2I_TYPE : ILLEGAL_TYPE )

#define OR3OK(x,y,z) ( ((x)&6) + ((y)&6) + ((z)&6) == 6 )

      if( strncmp(argv[iarg],"-orient",4) == 0 ){
         char acod ;

         if( iarg+1 >= argc ) SynErr("need an argument after -orient!");

         MCW_strncpy(orient_code,argv[++iarg],4) ;
         if( strlen(orient_code) != 3 ) SynErr("Illegal -orient code") ;

         acod = toupper(orient_code[0]) ; xxor = ORCODE(acod) ;
         acod = toupper(orient_code[1]) ; yyor = ORCODE(acod) ;
         acod = toupper(orient_code[2]) ; zzor = ORCODE(acod) ;

        if( xxor<0 || yyor<0 || zzor<0 || ! OR3OK(xxor,yyor,zzor) )
           SynErr("Unusable -orient code!") ;

         new_orient = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** -?origin dist **/

      if( strcmp(argv[iarg],"-xorigin") == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -xorigin!");
         if( strncmp(argv[iarg],"cen",3) == 0 ) cxorg = 1 ;
         else                                   xorg  = strtod(argv[iarg],NULL) ;
         dxorg = 0 ; new_xorg = 1 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-yorigin") == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -yorigin!");
         if( strncmp(argv[iarg],"cen",3) == 0 ) cyorg = 1 ;
         else                                   yorg  = strtod(argv[iarg],NULL) ;
         dyorg = 0 ; new_yorg = 1 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-zorigin") == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -zorigin!");
         if( strncmp(argv[iarg],"cen",3) == 0 ) czorg = 1 ;
         else                                   zorg  = strtod(argv[iarg],NULL) ;
         dzorg = 0 ; new_zorg = 1 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      /* 13 Sep 2000: -duporigin */

      if( strcmp(argv[iarg],"-duporigin") == 0 ){
         THD_3dim_dataset * cset ;
         if( ++iarg >= argc ) SynErr("need an argument after -duporigin!");
         cset = THD_open_dataset( argv[iarg] ) ;
         if( cset == NULL ) SynErr("couldn't open -duporigin dataset!");
         daxes = cset->daxes ;
         xorg = daxes->xxorg ; yorg = daxes->yyorg ; zorg = daxes->zzorg ;
         cxorg = cyorg = czorg = dxorg = dyorg = dzorg = 0 ;
         new_xorg = new_yorg = new_zorg = duporg = 1 ; new_stuff++ ;
         DSET_delete(cset) ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      /* 02 Mar 2000: -d?origin stuff, to go with plug_nudge.c */

      if( strncmp(argv[iarg],"-dxorigin",4) == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -dxorigin!");
         xorg = strtod(argv[iarg],NULL) ; dxorg = 1 ; cxorg = 0 ;
         new_xorg = 1 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-dyorigin",4) == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -dyorigin!");
         yorg = strtod(argv[iarg],NULL) ; dyorg = 1 ; cyorg = 0 ;
         new_yorg = 1 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-dzorigin",4) == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -dzorigin!");
         zorg = strtod(argv[iarg],NULL) ; dzorg = 1 ; czorg = 0 ;
         new_zorg = 1 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** 04 Oct 2002: _raw origins **/

      if( strcmp(argv[iarg],"-xorigin_raw") == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -xorigin_raw!");
         xorg     = strtod(argv[iarg],NULL) ; cxorg = dxorg = 0 ;
         new_xorg = 2 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-yorigin_raw") == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -yorigin_raw!");
         yorg     = strtod(argv[iarg],NULL) ; cyorg = dyorg = 0 ;
         new_yorg = 2 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-zorigin_raw") == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -zorigin_raw!");
         zorg     = strtod(argv[iarg],NULL) ; czorg = dzorg = 0 ;
         new_zorg = 2 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** 04 Oct 2002: zadd VOLREG fields **/
      if( strcmp(argv[iarg],"-vr_mat") == 0 ){
         if( iarg+12 >= argc ) SynErr("need 12 arguments after -vr_mat!");
         icnt = 0;
         while (icnt < 12) {
            ++iarg;
            volreg_mat[icnt] = strtod(argv[iarg], &lcpt) ; if (*lcpt != '\0') SynErr("Bad syntax in list of numbers!");
            ++icnt;
         }
         Do_volreg_mat = 1; new_stuff++ ;
         ++iarg;
         continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-vr_mat_ind") == 0) {
         if (++iarg >= argc) SynErr("need 1 argument after -vr_mat_ind!");
         volreg_matind = (int)strtol(argv[iarg], &lcpt, 10); if (*lcpt != '\0') SynErr("Bad syntax in number argument!");
         ++iarg;
         continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-vr_center_old") == 0) {
         if (iarg+3 >= argc) SynErr("need 3 arguments after -vr_center_old");
         ++iarg;
         center_old[0] = strtod(argv[iarg],&lcpt) ; ++iarg; if (*lcpt != '\0') SynErr("Bad syntax in list of numbers!");
         center_old[1] = strtod(argv[iarg],&lcpt) ; ++iarg; if (*lcpt != '\0') SynErr("Bad syntax in list of numbers!");
         center_old[2] = strtod(argv[iarg],&lcpt) ;  if (*lcpt != '\0') SynErr("Bad syntax in list of numbers!");
         Do_center_old = 1; new_stuff++ ;
         ++iarg;
         continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-vr_center_base") == 0) {
         if (iarg+3 >= argc) SynErr("need 3 arguments after -vr_center_base");
         ++iarg;
         center_base[0] = strtod(argv[iarg],&lcpt) ; ++iarg; if (*lcpt != '\0') SynErr("Bad syntax in list of numbers!");
         center_base[1] = strtod(argv[iarg],&lcpt) ; ++iarg; if (*lcpt != '\0') SynErr("Bad syntax in list of numbers!");
         center_base[2] = strtod(argv[iarg],&lcpt) ;  if (*lcpt != '\0') SynErr("Bad syntax in list of numbers!");
         Do_center_base = 1; new_stuff++ ;
         ++iarg;
         continue ;  /* go to next arg */
      }

      /** -?del dim **/

      if( strncmp(argv[iarg],"-xdel",4) == 0 ){
         if( iarg+1 >= argc ) SynErr("need an argument after -xdel!");
         xdel = strtod( argv[++iarg]  , NULL ) ;
         if( xdel <= 0.0 ) SynErr("argument after -xdel must be positive!") ;
         new_xdel = 1 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-ydel",4) == 0 ){
         if( iarg+1 >= argc ) SynErr("need an argument after -ydel!");
         ydel = strtod( argv[++iarg]  , NULL ) ;
         if( ydel <= 0.0 ) SynErr("argument after -ydel must be positive!") ;
         new_ydel = 1 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-zdel",4) == 0 ){
         if( iarg+1 >= argc ) SynErr("need an argument after -zdel!");
         zdel = strtod( argv[++iarg]  , NULL ) ;
         if( zdel <= 0.0 ) SynErr("argument after -zdel must be positive!") ;
         new_zdel = 1 ; new_stuff++ ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-keepcen",7) == 0 ){  /* 17 Jul 2006 */
         keepcen = 1 ;
         geom_change = 1;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-verb") == 0 ){
         verb++ ; iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-quiet") == 0 ){ /* 18 Dec 2017 */
         verb = 0 ; iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-xyzscale",8) == 0 ){ /* 17 Jul 2006 */
         if( iarg+1 >= argc ) SynErr("need an argument after -xyzscale!");
         xyzscale = strtod( argv[++iarg] , NULL ) ;
         if( xyzscale <= 0.0f ) SynErr("argument after -xyzscale must be positive!");
         if( xyzscale == 1.0f )
           WARNING_message(
            "-xyzscale 1.0 really makes no sense, but if that's what you want" ) ;
         geom_change = 1;
         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /** -TR **/

      if( strncmp(argv[iarg],"-TR",3) == 0 ){
         char *eptr = "\0" ;
         if( ++iarg >= argc ) SynErr("need an argument after -TR!");

         if( isalpha(argv[iarg][0])             ||
             strstr(argv[iarg],"+orig") != NULL ||
             strstr(argv[iarg],"+tlrc") != NULL ||
             strstr(argv[iarg],".nii")  != NULL    ){
           THD_3dim_dataset *qset = THD_open_dataset(argv[iarg]) ;
           if( qset != NULL ){
             DSET_UNMSEC(qset) ;
             TR = DSET_TR(qset); tunits = DSET_TIMEUNITS(qset);
             DSET_delete(qset) ; new_tunits = (tunits != ILLEGAL_TYPE) ;
             if( verb && TR > 0.0f ) INFO_message("new TR will be %g",TR) ;
           } else {
             ERROR_exit("-TR: can't open '%s' as a dataset",argv[iarg]) ;
           }
         } else {
           TR = strtod( argv[iarg]  , &eptr ) ;
         }
         if( TR <= 0.0 ) SynErr("argument after -TR must give a positive result!") ;

         if( strcmp(eptr,"ms")==0 || strcmp(eptr,"msec")==0 ){
            new_tunits = 1 ; tunits = UNITS_MSEC_TYPE ;
            ERROR_exit("TR expressed in milliseconds is no longer allowed.") ;
         } else if( strcmp(eptr,"s")==0 || strcmp(eptr,"sec")==0 ){
            new_tunits = 1 ; tunits = UNITS_SEC_TYPE ;
         } else if( strcmp(eptr,"Hz")==0 || strcmp(eptr,"Hertz")==0 ){
            new_tunits = 1 ; tunits = UNITS_HZ_TYPE ;
         }

         new_TR = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** -Tslices [18 Dec 2018] **/

#define IS_NUMERIC(sss)                                           \
 ( (                                       isdigit((sss)[0]) ) || \
   ( (sss)[0] == '.'                    && isdigit((sss)[1]) ) || \
   ( (sss)[0] == '-'                    && isdigit((sss)[1]) ) || \
   ( (sss)[0] == '-' && (sss)[1] == '.' && isdigit((sss)[2]) )   )

      if( strcasecmp(argv[iarg],"-Tslices") == 0 ){
        int ival ; float val , fac=1.0f ; char *cpt , *thisopt=argv[iarg];
         if( new_toff_sl > 0 )
           ERROR_exit("You cannot use -notoff and -Tslices in the same 3drefit command!") ;
        if( new_Tslices > 0 )
          ERROR_exit("You cannot uses option '%s' twice!",argv[iarg]) ;
        if( ++iarg >= argc )
          ERROR_exit("Option '%s' cannot be the last value on command line!",thisopt) ;
        if( argv[iarg][0] == '*' && IS_NUMERIC(argv[iarg]+1) ){
          fac = (float)strtod(argv[iarg]+1,&cpt ) ;
          if( fac <= 0.0f )
            ERROR_exit("Factor '%s' after option '%s' is illegal :(",argv[iarg],thisopt) ;
          iarg++ ;
#if 0
INFO_message("Set %s factor to %g",thisopt,fac) ;
#endif
        }
        for( ival=0 ; iarg+ival < argc && IS_NUMERIC(argv[iarg+ival]) ; ival++ ){
          val = (float)strtod(argv[iarg+ival], &cpt ) ;
          if( cpt == argv[iarg+ival] ) break ;  /* something bad */
          Tslices = (float *)realloc( Tslices , sizeof(float)*(ival+1) ) ;
          Tslices[ival] = fac * val ;
          if( val < 0.0f )
            WARNING_message("Value '%s' (#%d after option '%s') is negative :(",
                            argv[iarg+ival] , iarg+1 , thisopt ) ;
        }
        new_Tslices = ival ;
        if( new_Tslices == 0 )
          ERROR_exit("No number values found after option '%s' :(",thisopt) ;
        else if( new_Tslices == 1 )
          ERROR_exit("Only one number value found after option '%s' :(",thisopt) ;
#if 0
{ int qq ; fprintf(stderr,"%s values:",thisopt) ;
for( qq=0 ; qq < new_Tslices ; qq++ ) fprintf(stderr," %g",Tslices[qq]) ;
fprintf(stderr,"\n") ; }
#endif
        iarg += ival ; new_stuff++ ;continue ;
      }

      /** -notoff (12 Feb 2001) **/

      if( strncmp(argv[iarg],"-notoff",7) == 0 ){
         if( new_Tslices > 0 )
           ERROR_exit("You cannot use -notoff and -Tslices in the same 3drefit command!") ;
         new_toff_sl = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** -Torg (29 Jan 2003) **/

      if( strncmp(argv[iarg],"-Torg",5) == 0 ){
        char *eptr ;
        if( iarg+1 >= argc ) SynErr("need an argument after -Torg!");
        Torg = strtod( argv[++iarg]  , &eptr ) ;
        if( *eptr != '\0' )
          WARNING_message("-Torg %s ends in unexpected character\n",argv[iarg]) ;
        new_Torg = 1 ; new_stuff++ ;
        iarg++ ; continue ;  /* go to next arg */
      }

      /** -newid **/

      if( strncmp(argv[iarg],"-newid",4) == 0 ){
         new_idcode = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** -nowarp **/

      if( strncmp(argv[iarg],"-nowarp",6) == 0 ){
         new_nowarp = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** -statpar x x x **/

      if( strncmp(argv[iarg],"-statpar",4) == 0 ){
         float val ; char * ptr ;

         if( ++iarg >= argc ) SynErr("need an argument after -statpar!") ;

         for( ii=0 ; ii < MAX_STAT_AUX ; ii++ ) stataux[ii] = 0.0 ;

         ii = 0 ;
         do{
            val = strtod( argv[iarg] , &ptr ) ;
            if( *ptr != '\0' ) break ;
            stataux[ii++] = val ;
            iarg++ ;
         } while( iarg < argc ) ;

         if( ii == 0 ) SynErr("No numbers given after -statpar?") ;

         new_stataux = 1 ; new_stuff++ ;
         continue ;
      }

      /** -markers **/

      if( strncmp(argv[iarg],"-markers",4) == 0 ){
         new_markers = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** -label2 [21 Dec 2004] **/

      if( strcmp(argv[iarg],"-label2") == 0 ){
        new_label2 = argv[++iarg] ; new_stuff++ ;
        iarg++ ; continue ;  /* go to next arg */
      }

      /** -labeltable [25 Feb 2010 ZSS] **/

      if( strcmp(argv[iarg],"-labeltable") == 0 ){
        labeltable = argv[++iarg] ; new_stuff++ ;
        iarg++ ; continue ;  /* go to next arg */
      }

      /** -view code **/

      if( strncmp(argv[iarg],"-view",4) == 0 ){
         char * code ;
         if( iarg+1 >= argc ) SynErr("need an argument after -view!") ;
         code = argv[++iarg] ; if( code[0] == '+' ) code++ ;
         for( vtype=0 ; vtype <= LAST_VIEW_TYPE ; vtype++ )
            if( strcmp(code,VIEW_codestr[vtype]) == 0 ) break ;
         if( vtype > LAST_VIEW_TYPE ) SynErr("argument after -view is illegal!") ;
         new_view = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /* tag options    08 May 2006 [rickr] */

      /* -shift_tags, apply -d?origin to tags */
      if( strncmp(argv[iarg],"-shift_tags",11) == 0 ){
         shift_tags = 1 ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-dxtag",6) == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -dxtag!");
         dxtag = strtod(argv[iarg],NULL) ;
         new_tags = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-dytag",6) == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -dytag!");
         dytag = strtod(argv[iarg],NULL) ;
         new_tags = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-dztag",6) == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -dztag!");
         dztag = strtod(argv[iarg],NULL) ;
         new_tags = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -deoblique option [20 Jun 2007] -----*/

      if( strcmp(argv[iarg],"-deoblique") == 0 ){
         deoblique = 1 ;
         THD_set_oblique_report(0,0); /* turn off obliquity warning */
         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -oblique_origin option [01 Dec 2008] -----*/

      if( strcmp(argv[iarg],"-oblique_origin") == 0 ){
         use_oblique_origin = 1 ;
         THD_set_oblique_report(0,0); /* turn off obliquity warning */
         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -space option [16 Mar 2009] -----*/

      if( strcmp(argv[iarg],"-space") == 0 ){
         space = 1 ;
         spacename = argv[++iarg] ;
         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -cmap option [31 Mar 2009] -----*/
      if( strcmp(argv[iarg],"-cmap") == 0 ){
         if( ++iarg >= argc ) SynErr("need an argument after -cmap!");
         if(strcmp(argv[iarg],"CONT_CMAP")==0)
            cmap = CONT_CMAP;

         else {
            if(strcmp(argv[iarg],"INT_CMAP")==0) cmap = INT_CMAP;
            else {
               if(strcmp(argv[iarg],"SPARSE_CMAP")==0) cmap = SPARSE_CMAP;
               else SynErr("cmap value not valid");
            }
         }
         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /** anything else must be a -type **/
      /*  try the anatomy prefixes */

      for( ii=FIRST_ANAT_TYPE ; ii <= LAST_ANAT_TYPE ; ii++ )
         if( strncmp( &(argv[iarg][1]) ,
                      ANAT_prefixstr[ii] , THD_MAX_PREFIX ) == 0 ) break ;

      /* fprintf(stderr,"== have type %d\n", ii); */

      if( ii <= LAST_ANAT_TYPE ){
         ftype = ii ;
         dtype = HEAD_ANAT_TYPE ;
         nvals = ANAT_nvals[ftype] ;
         new_type = 1 ; new_stuff++ ;
         iarg++ ; continue ;
      }

      /* try the function prefixes */

      for( ii=FIRST_FUNC_TYPE ; ii <= LAST_FUNC_TYPE ; ii++ )
         if( strncmp( &(argv[iarg][1]) ,
                      FUNC_prefixstr[ii] , THD_MAX_PREFIX ) == 0 ) break ;

      if( ii <= LAST_FUNC_TYPE ){
         ftype = ii ;
         dtype = HEAD_FUNC_TYPE ;
         nvals = FUNC_nvals[ftype] ;
         new_type = 1 ; new_stuff++ ;
         iarg++ ; continue ;
      }

      /** error **/

      { char str[256] ;
        sprintf(str,"Unknown option %s. Check 3drefit -help.",argv[iarg]) ;
        ERROR_message(str);
        suggest_best_prog_option(argv[0], argv[iarg]);
        exit(1);
      }

   }  /* end of loop over switches */
   if (iarg < 2) Syntax(1) ;

   /*-- some checks for erroneous inputs --*/

   if( new_stuff == 0 && atrmod == 0 ) SynErr("No options given!?") ;
   if( new_stuff == 1 && atrmod == 1 ){       /* 28 Jul 2006 [rickr] */
      fprintf(stderr,"** Cannot use -atrcopy or -atrstring with other "
                     "modification options.\n");
      SynErr("Illegal attribute syntax.");
   }
   if( iarg >= argc   ) SynErr("No datasets given!?") ;

   if( xyzscale != 0.0f &&
       (new_orient || new_xorg || new_yorg || new_zorg ||
        keepcen    || new_xdel || new_ydel || new_zdel   ) ){  /* 18 Jul 2006 */
    SynErr(
    "-xyzscale is incompatible with other options for changing voxel grid");
   }

   if( new_orient && (dxorg || dyorg || dzorg) )     /* 02 Mar 2000 */
      SynErr("Can't use -orient with -d?origin!?") ;

   if( new_tags || shift_tags ){                     /* 08 May 2006 [rickr] */
      if( new_tags && shift_tags )
         SynErr("Cant' use -shift_tags with -d{xyz}tag") ;
      if( new_orient )
         SynErr("Can't use -orient with -shift_tags or -d{xyz}tags") ;
      if( shift_tags && !dxorg && !dyorg && !dzorg )
         SynErr("-shift_tags option requires a -d{xyz}origin option") ;

      if( shift_tags ){    /* then copy shifts to tag vars */
         if( dxorg ) dxtag = xorg ;
         if( dyorg ) dytag = yorg ;
         if( dzorg ) dztag = zorg ;
      }
   }

   /*--- process datasets ---*/
   for( ; iarg < argc ; iarg++ ){
      write_output = False ;   /* some datasets will be overwritten */
      did_something = 0 ;

      dset = THD_open_one_dataset( argv[iarg] ) ;
      if( dset == NULL ){
        ERROR_message("Can't open dataset %s\n",argv[iarg]) ;
        continue ;
      }
      if( DSET_IS_MINC(dset) ){
        ERROR_message("Can't process MINC dataset %s\n",argv[iarg]);
        continue ;
      }
      if( DSET_IS_ANALYZE(dset) ){
        ERROR_message("Can't process ANALYZE dataset %s\n",argv[iarg]);
        continue ;
      }
      if( DSET_IS_1D(dset) ){
        ERROR_message("Can't process 1D dataset %s\n",argv[iarg]);
        continue ;
      }
      if( DSET_IS_CTFMRI(dset) || DSET_IS_CTFSAM(dset) ){
        ERROR_message("Can't process CTF dataset %s\n",argv[iarg]);
        continue ;
      }
      if( DSET_IS_MPEG(dset) ){
        ERROR_message("Can't process MPEG dataset %s\n",argv[iarg]);
        continue ;
      }

      /* any surviving non-AFNI dataset needs the data written out */
      if( IS_VALID_NON_AFNI_DSET(dset) ){
        write_output = True ;     /* 13 Jul 2006 [rickr] */
      }

      INFO_message("Processing AFNI dataset %s\n",argv[iarg]) ;

      /*-- First off: check the axes for consistency --*/

      if( do_checkaxes ){
        THD_dataxes *copy_dax , *orig_dax ; float dif ;
        orig_dax = dset->daxes ;
        copy_dax = (THD_dataxes *)malloc(sizeof(THD_dataxes)) ;
        memcpy( copy_dax , orig_dax , sizeof(THD_dataxes) ) ;
        LOAD_ZERO_MAT( copy_dax->to_dicomm ) ;
        THD_daxes_to_mat44( copy_dax ) ;

        dif = MAT44_FLDIF( orig_dax->ijk_to_dicom , copy_dax->ijk_to_dicom ) ;
        if( dif > 0.001f ){
          WARNING_message("===== ijk_to_dicom from dataset header and from axes differ") ;
          DUMP_MAT44("ijk_to_dicom from dataset header",orig_dax->ijk_to_dicom) ;
          DUMP_MAT44("ijk_to_dicom from dataset axes"  ,copy_dax->ijk_to_dicom) ;
        } else {
          INFO_message   ("===== ijk_to_dicom from dataset header and from axes are equivalent") ;
          DUMP_MAT44("ijk_to_dicom from dataset header",orig_dax->ijk_to_dicom) ;
        }

        dif = MAT44_FLDIF( orig_dax->dicom_to_ijk , copy_dax->dicom_to_ijk ) ;
        if( dif > 0.001f ){
          WARNING_message("===== dicom_to_ijk from dataset header and from axes differ") ;
          DUMP_MAT44("dicom_to_ijk from dataset header",orig_dax->dicom_to_ijk) ;
          DUMP_MAT44("dicom_to_ijk from dataset axes"  ,copy_dax->dicom_to_ijk) ;
        } else {
          INFO_message   ("===== dicom_to_ijk from dataset header and from axes are equivalent") ;
          DUMP_MAT44("dicom_to_ijk from dataset header",orig_dax->dicom_to_ijk) ;
        }

        dif = MAT44_FLDIF( orig_dax->ijk_to_dicom , orig_dax->ijk_to_dicom_real ) ;
        if( dif > 0.001f ){
          WARNING_message("===== ijk_to_dicom and ijk_to_dicom_real from dataset header differ") ;
          DUMP_MAT44("ijk_to_dicom      from dataset header",orig_dax->ijk_to_dicom) ;
          DUMP_MAT44("ijk_to_dicom_real from dataset header",orig_dax->ijk_to_dicom_real) ;
        } else {
          DUMP_MAT44("ijk_to_dicom      from dataset header",orig_dax->ijk_to_dicom) ;
          INFO_message   ("===== ijk_to_dicom and ijk_to_dicom_real from dataset header are equivalent") ;
        }

        if( new_stuff == 1 ){ DSET_delete(dset) ; continue ; }  /* nothing else to do */
      }

      tross_Make_History( "3drefit" , argc,argv, dset ) ;

      /* 21 Dec 2004: -label2 option */

      if( new_label2 != NULL ){
        EDIT_dset_items( dset , ADN_label2 , new_label2 , ADN_none ) ;
        VINFO("setting label2") ;
        did_something++ ; /* 30 Mar 2010 */
      }

      if(labeltable != NULL) {
         char *str = NULL;
         Dtable *vl_dtable=NULL ;

         if (dset->Label_Dtable) {
            destroy_Dtable(dset->Label_Dtable); dset->Label_Dtable=NULL;
         }
         /* read the table */
         if (!(str = AFNI_suck_file( labeltable))) {
            ERROR_exit("Failed to read %s", labeltable);
         }
         if (!(vl_dtable = Dtable_from_nimlstring(str))) {
            ERROR_exit("Could not parse labeltable");
         }
         destroy_Dtable(vl_dtable); vl_dtable = NULL;
         THD_set_string_atr( dset->dblk , "VALUE_LABEL_DTABLE" , str ) ;
         VINFO("setting labeltable") ;
         free(str);
        did_something++ ; /* 30 Mar 2010 */
      }

      /* 14 Oct 1999: change anat parent */
      /* 14 Dec 1999: allow special cases: SELF and NULL */

      if( aset != NULL ){
         EDIT_dset_items( dset , ADN_anat_parent , aset , ADN_none ) ;
         VINFO("setting Anat parent") ;
        did_something++ ; /* 30 Mar 2010 */
      } else if( aset_code == ASET_SELF ){
         EDIT_dset_items( dset , ADN_anat_parent , dset , ADN_none ) ;
         VINFO("setting Anat parent") ;
        did_something++ ; /* 30 Mar 2010 */
      } else if( aset_code == ASET_NULL ){
         EDIT_ZERO_ANATOMY_PARENT_ID( dset ) ;
         dset->anat_parent_name[0] = '\0' ;
         VINFO("clearing Anat parent") ;
        did_something++ ; /* 30 Mar 2010 */
      }

      /* ZSS June 06, add a warp parent field please */
      if( waset != NULL ){
         EDIT_dset_items( dset , ADN_warp_parent , waset , ADN_none ) ;
         VINFO("setting Warp parent") ;
        did_something++ ; /* 30 Mar 2010 */
      } else if( waset_code == ASET_SELF ){
         EDIT_dset_items( dset , ADN_warp_parent , dset , ADN_none ) ;
         VINFO("setting Warp parent") ;
        did_something++ ; /* 30 Mar 2010 */
      } else if( waset_code == ASET_NULL ){
         EDIT_ZERO_ANATOMY_PARENT_ID( dset ) ;
         dset->warp_parent_name[0] = '\0' ;
         VINFO("clearing Warp parent") ;
        did_something++ ; /* 30 Mar 2010 */
      }
      /* Oct 04/02: zmodify volreg fields */
      if (Do_volreg_mat) {
         sprintf(str,"VOLREG_MATVEC_%06d", volreg_matind) ;
         if( verb ) ININFO_message("Modifying %s ...\n", str);
         THD_set_float_atr( dset->dblk , str , 12 , volreg_mat ) ;
        did_something++ ; /* 30 Mar 2010 */
      }

      if (Do_center_old) {
         VINFO("Modifying VOLREG_CENTER_OLD ...\n");
         THD_set_float_atr( dset->dblk , "VOLREG_CENTER_OLD" , 3 , center_old ) ;
        did_something++ ; /* 30 Mar 2010 */
      }

      if (Do_center_base) {
        VINFO("Modifying VOLREG_CENTER_BASE ...\n");
        THD_set_float_atr( dset->dblk , "VOLREG_CENTER_BASE" , 3 , center_base ) ;
        did_something++ ; /* 30 Mar 2010 */
      }

      /* 28 May 2002: clear brick stats */

      if( clear_bstat ){
        if( !ISVALID_STATISTIC(dset->stats) ){
          WARNING_message("-clear_bstat: dataset has no brick statistics\n") ;
        } else {
          KILL_STATISTIC(dset->stats) ;
          REMOVEFROM_KILL( dset->kl , dset->stats ) ;
          REMOVEFROM_KILL( dset->kl , dset->stats->bstat ) ;
          dset->stats = NULL ;
          VINFO("clearing brick statistics") ;
        did_something++ ; /* 30 Mar 2010 */
        }
      }

      if( redo_bstat ){
        VINFO("reloading brick statistics") ;
        THD_load_statistics( dset ) ;   /* 01 Feb 2005 */
        did_something++ ; /* 30 Mar 2010 */
      }

      if( new_byte_order > 0 ){
         VINFO("changing byte order") ;
         dset->dblk->diskptr->byte_order = new_byte_order ; /* 25 April 1998 */
        did_something++ ; /* 30 Mar 2010 */
      }

      /*-- change space axes (lots of possibilities here) --*/

      daxes = dset->daxes ;

      if( new_orient ){
         VINFO("changing orientation codes") ;
         daxes->xxorient = xxor ;
         daxes->yyorient = yyor ;
         daxes->zzorient = zzor ;
        did_something++ ; /* 30 Mar 2010 */
      }

      if( xyzscale > 0.0f ){  /* 18 Jul 2006 */
        float dxp = daxes->xxdel * xyzscale ;  /* new grid */
        float dyp = daxes->yydel * xyzscale ;  /* spacings */
        float dzp = daxes->zzdel * xyzscale ;
        int   rl  = abs(THD_get_axis_direction(daxes,ORI_R2L_TYPE)) ;
        int   ap  = abs(THD_get_axis_direction(daxes,ORI_A2P_TYPE)) ;
        int   is  = abs(THD_get_axis_direction(daxes,ORI_I2S_TYPE)) ;
        float xop , yop , zop ;
        static float shift[3] ;

        VINFO("applying -xyzscale") ;

        if( rl == 0 || ap == 0 || is == 0 )
          ERROR_exit("-xyzscale: Indeterminate axis directions!") ;

        if( ndone == 0 ){  /* for the first dataset */
          float op[3] , oo[3] ;
          op[0] = xop = daxes->xxorg + (daxes->xxdel-dxp)*0.5f*(daxes->nxx-1) ;
          op[1] = yop = daxes->yyorg + (daxes->yydel-dyp)*0.5f*(daxes->nyy-1) ;
          op[2] = zop = daxes->zzorg + (daxes->zzdel-dzp)*0.5f*(daxes->nzz-1) ;
          oo[0] = daxes->xxorg ;
          oo[1] = daxes->yyorg ;
          oo[2] = daxes->zzorg ;
          shift[0] = op[rl-1] - xyzscale * oo[rl-1] ;   /* RL shift */
          shift[1] = op[ap-1] - xyzscale * oo[ap-1] ;   /* AP shift */
          shift[2] = op[is-1] - xyzscale * oo[is-1] ;   /* IS shift */

        } else {           /* for later datasets */

          xop = xyzscale * daxes->xxorg + shift[daxes->xxorient/2] ;
          yop = xyzscale * daxes->yyorg + shift[daxes->yyorient/2] ;
          zop = xyzscale * daxes->zzorg + shift[daxes->zzorient/2] ;
        }

        daxes->xxdel = dxp ; daxes->yydel = dyp ; daxes->zzdel = dzp ;
        daxes->xxorg = xop ; daxes->yyorg = yop ; daxes->zzorg = zop ;
        did_something++ ; /* 30 Mar 2010 */
      }

      if( !new_xorg ) xorg = fabs(daxes->xxorg) ;
      if( !new_yorg ) yorg = fabs(daxes->yyorg) ;
      if( !new_zorg ) zorg = fabs(daxes->zzorg) ;

      if( !new_xdel ) xdel = fabs(daxes->xxdel) ;
      if( !new_ydel ) ydel = fabs(daxes->yydel) ;
      if( !new_zdel ) zdel = fabs(daxes->zzdel) ;

      /* 17 Jul 2006 - deal with the '-keepcen' option */

      if( keepcen && !new_xdel && !new_ydel && !new_zdel ){
        WARNING_message("-keepcen needs at least one of -xdel, -ydel, -zdel") ;
        keepcen = 0 ;
      }
      if( keepcen && (new_xorg || new_yorg || new_zorg || new_orient) ){
        WARNING_message(
         "-keepcen incompatible with explicit origin or orientation changes") ;
        keepcen = 0 ;
      }
      if( keepcen ){
        VINFO("applying -keepcen") ;
        if( new_xdel ){
          dxorg = 1 ; xorg = 0.5f*(daxes->nxx-1)*(fabs(daxes->xxdel)-xdel) ;
          if( ORIENT_sign[daxes->xxorient] == '-' ) xorg = -xorg ;
        }
        if( new_ydel ){
          dyorg = 1 ; yorg = 0.5f*(daxes->nyy-1)*(fabs(daxes->yydel)-ydel) ;
          if( ORIENT_sign[daxes->yyorient] == '-' ) yorg = -yorg ;
        }
        if( new_zdel ){
          dzorg = 1 ; zorg = 0.5f*(daxes->nzz-1)*(fabs(daxes->zzdel)-zdel) ;
          if( ORIENT_sign[daxes->zzorient] == '-' ) zorg = -zorg ;
        }
      }

      if( cxorg ) xorg = 0.5 * (daxes->nxx - 1) * xdel ;
      if( cyorg ) yorg = 0.5 * (daxes->nyy - 1) * ydel ;
      if( czorg ) zorg = 0.5 * (daxes->nzz - 1) * zdel ;

      if( dxorg )
      {  daxes->xxorg += xorg ; did_something++ ; }
      else if( duporg || new_xorg==2 )
      {  daxes->xxorg = xorg ; did_something++ ; }
      else if( new_xorg==1 || new_orient )
      {  daxes->xxorg = (ORIENT_sign[daxes->xxorient] == '+') ? (-xorg) : (xorg) ; did_something++ ; }

      if( dyorg )
      {  daxes->yyorg += yorg ; did_something++ ; }
      else if( duporg || new_yorg==2 )
      {  daxes->yyorg = yorg ; did_something++ ; }
      else if( new_yorg==1 || new_orient )
      {  daxes->yyorg = (ORIENT_sign[daxes->yyorient] == '+') ? (-yorg) : (yorg) ; did_something++ ; }

      if( dzorg )
      {  daxes->zzorg += zorg ; did_something++ ; }
      else if( duporg || new_zorg==2 )
      {  daxes->zzorg = zorg ; did_something++ ; }
      else if( new_zorg==1 || new_orient )
      {  daxes->zzorg = (ORIENT_sign[daxes->zzorient] == '+') ? (-zorg) : (zorg) ; did_something++ ; }

      if( new_xdel || new_orient )
      {  daxes->xxdel = (ORIENT_sign[daxes->xxorient] == '+') ? (xdel) : (-xdel) ; did_something++ ; }

      if( new_ydel || new_orient )
      {  daxes->yydel = (ORIENT_sign[daxes->yyorient] == '+') ? (ydel) : (-ydel) ; did_something++ ; }

      if( new_zdel || new_orient )
      {  daxes->zzdel = (ORIENT_sign[daxes->zzorient] == '+') ? (zdel) : (-zdel) ; did_something++ ; }

      /*-- deoblique - assume the data is cardinal  6/20/2007 */
      /* this should be after any other axis,
         orientation, origin, voxel size changes */
      if(deoblique || geom_change) {   /* geom_change  04 Nov 2011 mod drg */
         /* replace transformation matrix with cardinal form */
         /* lose obliquity if using 3dWarp for any transformation */
         /* recompute Tc (Cardinal transformation matrix for new grid output */
         THD_make_cardinal(dset);
         VINFO("deoblique") ;
         did_something++ ; /* 30 Mar 2010 */
      }


      /* if user has selected, get origin from obliquity */
      /*   overriding all the previous command-line options */
      if(use_oblique_origin){
         Obliquity_to_coords(dset);
         VINFO("oblique origin") ;
        did_something++ ; /* 30 Mar 2010 */
      }

      /* set the space of the dataset */
      if(space) {
         int old_vtype = dset->view_type ;
         /* check if trying to assign a non-orig space to orig view data */
         if( strcmp("orig",VIEW_codestr[old_vtype]) == 0 ) {
            if(strncmp(spacename, "ORIG", 4)!=0){
               WARNING_message("Changing the space of an ORIG view dataset may cause confusion!");
               WARNING_message(" NIFTI copies will be interpreted as TLRC view (not TLRC space).");
               WARNING_message(" Consider changing the view of the dataset to TLRC view also");
            }
         }
         /* check if trying to assign orig space to tlrc view data */
         else if( strcmp("tlrc",VIEW_codestr[old_vtype]) == 0 ) {
            if(strncmp(spacename, "ORIG", 4)==0){
               WARNING_message("Changing the space of a TLRC view dataset to an ORIG type may cause confusion!");
               WARNING_message(" NIFTI copies will be interpreted as ORIG view.");
               WARNING_message(" Consider changing the view of the dataset to ORIG view also");
            }
         }
         /* actually update the space */
         MCW_strncpy(dset->atlas_space, spacename, THD_MAX_NAME);
         did_something++;
      }

      /* set the colormap type of the dataset */
      if(cmap>=0)
      {
            dset->int_cmap = cmap;
            did_something++;
      }

      /*-- change time axis --*/

      if( new_TR ){
         if( dset->taxis == NULL ){
            if( DSET_NVALS(dset) < 2 ){
              WARNING_message("Can't process -TR for this dataset!") ;
            } else {
              WARNING_message("Adding time axis to this dataset") ;
              EDIT_dset_items( dset ,
                                 ADN_ntt   , DSET_NVALS(dset) ,
                                 ADN_ttdel , TR ,
                                 ADN_tunits, UNITS_SEC_TYPE ,
                                 ADN_nsl   , 0 ,
                               ADN_none ) ;
              did_something++ ; /* 30 Mar 2010 */
            }
         } else {
            float frac = TR / dset->taxis->ttdel ;
            int ii ;

            VINFO("changing TR") ;
            dset->taxis->ttdel = TR ;
            if( new_tunits ) dset->taxis->units_type = tunits ;
            if( dset->taxis->nsl > 0 ){
               for( ii=0 ; ii < dset->taxis->nsl ; ii++ )
                  dset->taxis->toff_sl[ii] *= frac ;
            }
            did_something++ ; /* 30 Mar 2010 */
         }
      }

      if( new_Torg ){                   /* 29 Jan 2003 */
        if( dset->taxis == NULL ){
          WARNING_message("Can't process -Torg for this dataset!\n") ;
        } else {
          VINFO("changing Torg") ;
          dset->taxis->ttorg = Torg ;
          did_something++ ; /* 30 Mar 2010 */
        }
      }

      if( new_toff_sl ){              /* 12 Feb 2001 */
         if( dset->taxis == NULL ){
            WARNING_message("-notoff: dataset has no time axis to clear!\n") ;
         } else if( dset->taxis->nsl <= 0 ){
            WARNING_message("-notoff: dataset has no time-offsets to clear!\n") ;
         } else {
            VINFO("clearing slice time offsets") ;
            EDIT_dset_items( dset , ADN_nsl,0 , ADN_none ) ;
            did_something++ ; /* 30 Mar 2010 */
         }
      }

      if( new_Tslices > 0 ){          /* 18 Dec 2018 */
         if( dset->taxis == NULL ){
           WARNING_message("-Tslices: dataset has no time axis to add slice offset to!") ;
         } else {
           int qq ; float dt=DSET_TR(dset) ;
           if( dset->taxis->nsl > 0 )
             WARNING_message("-Tslices: altering existing slice offsets!") ;
           else
             VINFO("setting slice time offsets") ;
           EDIT_dset_items( dset ,
                              ADN_nsl     , new_Tslices ,
                              ADN_toff_sl , Tslices     ,
                            ADN_none ) ;
           if( new_Tslices != dset->daxes->nzz )
             WARNING_message("-Tslices count %d is different than number of slices %d",
                             new_Tslices , dset->daxes->nzz ) ;
           for( qq=0 ; qq < new_Tslices ; qq++ ){
             if( Tslices[qq] >= dt )
               WARNING_message("-Tslices value %g (#%d) is %s TR=%g",
                               Tslices[qq] , qq+1 ,
                               (Tslices[qq] == dt) ? "equal to" : "greater than" ,
                               dt ) ;
           }
           did_something++ ;
         }
      }

      if( (new_orient || new_zorg) && dset->taxis != NULL && dset->taxis->nsl > 0 ){
         VINFO("changing time axis slice offset z-origin") ;
         dset->taxis->zorg_sl = daxes->zzorg ;
         did_something++ ; /* 30 Mar 2010 */
      }

      if( (new_orient || new_zdel) && dset->taxis != NULL && dset->taxis->nsl > 0 ){
         VINFO("changing time axis slice offset z-spacing") ;
         dset->taxis->dz_sl = daxes->zzdel ;
         did_something++ ; /* 30 Mar 2010 */
      }

      if( new_idcode ){
        VINFO("changing ID code") ;
        dset->idcode = MCW_new_idcode() ;
        did_something++ ; /* 30 Mar 2010 */
      }

      if( new_nowarp ){
         VINFO("clearing warp") ;
         ZERO_IDCODE( dset->warp_parent_idcode ) ;
         dset->warp_parent_name[0] = '\0' ;
         dset->warp = NULL ;
         did_something++ ; /* 30 Mar 2010 */
      }

      if( new_type ){
#if 0
/* removed these tests where nvals is used as number of values per sub-brick
   instead of number of sub-bricks. Apparently from another era, these are
   limited to a value of 1 or 2 in 3ddata.h for each data type */
/*          if( nvals > 1 && dset->taxis != NULL ){
            ERROR_message("Can't change 3D+time dataset to new type:\n"
                          " *    new type has more than one value per voxel!\n") ;
         } else if( dset->taxis == NULL && nvals != dset->dblk->nvals &&
                    ((dtype==HEAD_FUNC_TYPE && ftype!=FUNC_BUCK_TYPE)||
                     (dtype==HEAD_ANAT_TYPE && ftype!=ANAT_BUCK_TYPE)  ) ){

            ERROR_message("Can't change dataset to new type:\n"
                          " *     mismatch in number of sub-bricks!\n") ;
         } else {
 */
#endif
            VINFO("changing dataset 'type' marker") ;
            dset->type      = dtype ;
            dset->func_type = ftype ;

            if( ISBUCKET(dset) && dset->taxis != NULL ){   /* 29 April 1998 */
              WARNING_message("changing 3D+time dataset to bucket [no time axis]\n") ;
              EDIT_dset_items( dset , ADN_ntt , 0 , ADN_none ) ;
            }

            did_something++ ; /* set either way   17 Nov 2011 [rickr, dglen] */
/*         }*/
      }

      if( new_stataux ){
         for( ii=0 ; ii < MAX_STAT_AUX ; ii++ ){
            dset->stat_aux[ii] = stataux[ii] ;
         }
         did_something++ ; /* 30 Mar 2010 */
         VINFO("new stataux") ;
      }

      if( new_view && dset->view_type != vtype ){
         int  old_vtype = dset->view_type ;
         char old_head[THD_MAX_NAME] , old_brik[THD_MAX_NAME] ;
         char new_head[THD_MAX_NAME] , new_brik[THD_MAX_NAME] ;
         int brick_ccode = COMPRESS_filecode( DSET_BRIKNAME(dset) ) ;

         VINFO("changing dataset view code") ;
         strcpy(old_head,DSET_HEADNAME(dset)) ;
         strcpy(old_brik,DSET_BRIKNAME(dset)) ;
         dset->view_type = vtype ;
         THD_init_diskptr_names( dset->dblk->diskptr ,
                                 NULL , NULL , NULL , vtype , True ) ;

         #if 0 /* HEADNAME now has path, no need for catenation ZSS Fev 2012 */
         strcpy(new_head,DSET_DIRNAME(dset)) ;
         strcat(new_head,DSET_HEADNAME(dset)) ;
         strcpy(new_brik,DSET_DIRNAME(dset)) ;
         strcat(new_brik,DSET_BRIKNAME(dset)) ;
         #else
         strcpy(new_head,DSET_HEADNAME(dset)) ;
         strcpy(new_brik,DSET_BRIKNAME(dset)) ;
         #endif

         if( THD_is_file(new_head) ){
            dset->view_type = old_vtype ;
            THD_init_diskptr_names( dset->dblk->diskptr ,
                                    NULL , NULL , NULL , old_vtype , True ) ;
            /* if not changing the current file, fail
               (i.e. accept in case of NIfTI or similar in -space)
               (suggested by I Schwabacher)         24 Apr 2013 [rickr] */
            if( strcmp(old_head, new_head) )
             ERROR_exit("Can't change view: would overwrite existing files!\n");
         } else {
            rename( old_head , new_head ) ;
            { char * fff = COMPRESS_filename(old_brik) ;
              if( fff != NULL ){
                 char * ggg = malloc( sizeof(char) * (strlen(fff)+32) ) ;
                 strcpy(ggg,new_brik) ;
                 if( brick_ccode >= 0 ) strcat(ggg,COMPRESS_suffix[brick_ccode]) ;
                 rename( fff , ggg ) ;
                 free(fff) ; free(ggg) ;
              }
            }
            ININFO_message("Changed dataset view type and filenames.\n") ;
            did_something++ ; /* 30 Mar 2010 */
         }
      }

      /* check for tag shifts                  08 May 2006 [rickr] */
      if( new_tags || shift_tags ){
         THD_usertag * tag;
         if( !dset->tagset ) WARNING_message("No tags to shift\n") ;
         else {
            ININFO_message("modifying tags") ;
            for( ii = 0; ii < dset->tagset->num; ii++ ){
               tag = dset->tagset->tag + ii ;
               tag->x += dxtag;  tag->y += dytag;  tag->z += dztag;
            }
            did_something++ ; /* 30 Mar 2010 */
         }
      } else if ( dset->tagset && ( new_xorg || new_yorg || new_zorg ||
                                    new_xdel || new_ydel || new_zdel ) )
         WARNING_message("modifying coordinates of dataset with tags") ;

      /* code moved to edt_emptycopy.c                   13 Sep 2005 [rickr] */
      if( new_markers && okay_to_add_markers(dset) ){
         dset->markers = create_empty_marker_set() ;
         did_something++ ; /* 30 Mar 2010 */
         VINFO("empty marker set") ;

      } else if( new_markers ){
         WARNING_message("Can't add markers to this dataset\n") ;
      } /* end of markers */

      /*-- 08 Jun 2004: copyaux? --*/

      if( copyaux ){
        if( auxset != NULL ){
          THD_copy_datablock_auxdata( auxset->dblk , dset->dblk );
          INIT_STAT_AUX( dset , MAX_STAT_AUX , auxset->stat_aux ) ;
          did_something++ ; /* 30 Mar 2010 */
          VINFO("copy auxdata") ;
        } else {
          THD_copy_datablock_auxdata( NULL , dset->dblk );
          VINFO("null auxdata") ;
          did_something++ ; /* 30 Mar 2010 */
        }
      }

      /*-- 11 Jan 2012: copytables? --*/

      if( copytabs ){
        if( tabset != NULL ){
          if (!THD_copy_labeltable_atr( dset->dblk , tabset->dblk )) {
            WARNING_message("Failed to copy labletable attributes");
          }
          did_something++ ;
          VINFO("copy tabledata") ;
        }
      }


      /*-- relabel_all? [18 Apr 2011] --*/

      if( sar_relab != NULL ){
        for( ii=0 ; ii < sar_relab->num && ii < DSET_NVALS(dset) ; ii++ ){
          if( sar_relab->str[ii][0] != '\0' ){
            EDIT_BRICK_LABEL( dset , ii , sar_relab->str[ii] ) ;
            did_something++ ;
          }
        }
        VINFO("relabel_all") ;
      }

      /*-- new aux data? --*/

      if( nsublab > 0 ){
         for( ii=0 ; ii < nsublab ; ii++ ){
            iv = sublab[ii].iv ;
            if( iv < 0 || iv >= DSET_NVALS(dset) ){
               WARNING_message("Can't put label on sub-brick %d\n",iv) ;
            } else {
               EDIT_dset_items( dset ,
                                   ADN_brick_label_one + iv , sublab[ii].lab ,
                                ADN_none ) ;
               did_something++ ; /* 30 Mar 2010 */
               VINFO("edit sub-brick label") ;
            }
         }
      }

      /* add suffix to labels? */
      if (subsuff) {
         char *olab=NULL, *sss=NULL;
         for( ii=0 ; ii < DSET_NVALS(dset) ; ii++ ){
            olab = DSET_BRICK_LABEL(dset,ii); if (!olab) { olab = ""; }
            sss = (char *)calloc(strlen(olab)+strlen(subsuff)+1, sizeof(char));
            sprintf(sss,"%s%s", olab, subsuff);
            EDIT_BRICK_LABEL( dset , ii , sss ) ;
            free(sss);
            did_something++ ;
         }
         VINFO("sublabel_suffix") ;
      }

      /* add prefix to labels? */
      if (subpref) {
         char *olab=NULL, *sss=NULL;
         for( ii=0 ; ii < DSET_NVALS(dset) ; ii++ ){
            olab = DSET_BRICK_LABEL(dset,ii); if (!olab) { olab = ""; }
            sss = (char *)calloc(strlen(olab)+strlen(subpref)+1, sizeof(char));
            sprintf(sss,"%s%s", subpref, olab);
            EDIT_BRICK_LABEL( dset , ii , sss ) ;
            free(sss);
            did_something++ ;
         }
         VINFO("sublabel_prefix") ;
      }

      if( nsubkeyword > 0 ){
         int code ;
         for( ii=0 ; ii < nsubkeyword ; ii++ ){
            iv = subkeyword[ii].iv ; code = subkeyword[ii].code ;
            if( iv < 0 || iv >= DSET_NVALS(dset) ){
               WARNING_message("Can't put keyword on sub-brick %d\n",iv) ;
            } else if( code == 1 ){
               EDIT_dset_items( dset ,
                                   ADN_brick_keywords_append_one + iv ,
                                   subkeyword[ii].keyword ,
                                ADN_none ) ;
               did_something++ ; /* 30 Mar 2010 */
               VINFO("edit sub-brick keywords") ;
            } else if( code == 2 ){
               EDIT_dset_items( dset ,
                                   ADN_brick_keywords_replace_one + iv ,
                                   subkeyword[ii].keyword ,
                                ADN_none ) ;
               did_something++ ; /* 30 Mar 2010 */
               VINFO("edit sub-brick keywords") ;
            } else if( code == 3 && dset->dblk->brick_keywords != NULL ){
               EDIT_dset_items( dset ,
                                   ADN_brick_keywords_replace_one + iv ,
                                   NULL ,
                                ADN_none ) ;
               VINFO("nullify sub-brick keywords") ;
               did_something++ ; /* 30 Mar 2010 */
            }
         }
      }

      switch( new_key ){
        case 1: EDIT_dset_items(dset, ADN_keywords_append , key , ADN_none); did_something++; break;
        case 2: EDIT_dset_items(dset, ADN_keywords_replace, key , ADN_none); did_something++; break;
        case 3: EDIT_dset_items(dset, ADN_keywords_replace, NULL, ADN_none); did_something++; break;
      }

      if( do_killSTAT ){   /* 24 Jan 2008 */
        for( iv=0 ; iv < DSET_NVALS(dset) ; iv++ ){
          EDIT_BRICK_TO_NOSTAT(dset,iv) ;
        }
        VINFO("kill statistics") ;
        did_something++ ; /* 30 Mar 2010 */
      }

      if( nsubstatpar > 0 ){
        for( ii=0 ; ii < nsubstatpar ; ii++ ){
          iv = substatpar[ii].iv ;
          if( iv < 0 || iv >= DSET_NVALS(dset) ){
            WARNING_message("Can't put statpar on sub-brick %d",iv) ;
          } else {
            EDIT_dset_items( dset ,
                               ADN_brick_stataux_one + iv , substatpar[ii].par ,
                             ADN_none ) ;
            did_something++ ; /* 30 Mar 2010 */
            if( verb )
              INFO_message("changed statcode[%d] to %d",iv,(int)substatpar[ii].par[0]) ;
          }
        }
      }

      /* 03 Aug 2005: implement atrcopy */
      if( num_atrcopy > 0 )
      {
         ATR_any *atr;
         for( ii=0 ; ii < num_atrcopy ; ii++ ) {
           THD_insert_atr( dset->dblk , atrcopy[ii] ) ;
         }
         did_something++ ; /* 30 Mar 2010 */
         VINFO("atrcopy") ;
      }
      /* 23 Jan 2008: the FDR stuff */

      if( do_FDR ){
        DSET_BRICK_FDRCURVE_ALLKILL(dset) ;
        DSET_BRICK_MDFCURVE_ALLKILL(dset) ;  /* 22 Oct 2008 */
        if( do_FDR > 0 ){
          int nf ;
          mri_fdr_setmask( (nFDRmask == DSET_NVOX(dset)) ? FDRmask : NULL ) ;
          nf = THD_create_all_fdrcurves(dset) ;
          if( nf > 0 ){
            did_something += nf ; /* 30 Mar 2010 */
            ININFO_message("created %d FDR curve%s in dataset header",nf,(nf==1)?"\0":"s") ;
          } else {
            ININFO_message("failed to create FDR curves in dataset header") ;
          }
        }
      }

      /* Do we want to force new attributes into output ? ZSS Jun 06*/
      /* (only if -atrcopy or -atrstring)       28 Jul 2006 [rickr] */
      if ( saveatr && atrmod ){
         THD_set_dset_atr_status(0);
/*         THD_updating_obliquity(1);*/ /* allow the possibility to update the obliquity -
                                            otherwise gets overwritten with cardinal matrix in
                                            THD_set_dataset_attributes() */
         /* apply attributes to header - dataxes and dblk*/
         INFO_message("applying attributes");
         THD_datablock_from_atr(dset->dblk , DSET_DIRNAME(dset) ,
                                  dset->dblk->diskptr->header_name);
         THD_datablock_apply_atr(dset );
      }

      if( denote ){ THD_anonymize_write(1); did_something++; VINFO("denote");}   /* 08 Jul 2005 */

      if( !did_something ){
        ININFO_message("Didn't make any changes for dataset %s !",argv[iarg]) ;
      } else {
        if( write_output ) {
            ININFO_message(
               "loading and re-writing dataset %s (%s in %s storage)\n",
                  argv[iarg], dset->dblk->diskptr->header_name,
                  storage_mode_str(dset->dblk->diskptr->storage_mode) ) ;
            DSET_load(dset) ;    /* 20 Jun 2006 */
        }
        THD_force_ok_overwrite(1);             /* 24 Sep 2007 */
        THD_set_quiet_overwrite(1);
        THD_write_3dim_dataset( THD_filepath(argv[iarg]),NULL ,
                                dset , write_output ) ;
      }
      THD_delete_3dim_dataset( dset , False ) ;

      ndone++ ;   /* 18 Jul 2006: number of datasets done */

   } /* end of loop over datasets to be refitted */

   /*--- DONE ---*/

   INFO_message("3drefit processed %d datasets",ndone) ;
   exit(0) ;
}

/* read float values from string or file into float attribute */
static ATR_float *
Update_float_atr(char *aname, char *fvstring)
{
   ATR_float *atr ;
   MRI_IMAGE *mri_matrix = NULL;
   float *fptr;
   int nx, ny, nxy,ii, acount;

   ENTRY("Update_float_atr");
   if( !THD_filename_pure(aname) ){
     WARNING_message("Illegal atrfloat name %s",aname) ;
     RETURN(NULL) ;
   }

   atr = (ATR_float *)XtMalloc(sizeof(ATR_float)) ;
   atr->type = ATR_FLOAT_TYPE ;
   atr->name = XtNewString( aname ) ;

   /* parse floats from string to attribute */
   /* try reading as float file or 1D: expression */
   mri_matrix = mri_read_1D(fvstring);  /* string could be file name or commandline string */
   if (mri_matrix == NULL)   {
      mri_matrix = mri_1D_fromstring(fvstring);
   }

   if (mri_matrix == NULL)   {
      printf("Error reading floating point attribute file");
      RETURN(NULL);
   }

   /* number of floats in attribute */
   nx = mri_matrix->nx; ny = mri_matrix->ny; acount = nx*ny;
   atr->nfl  = acount ;
   atr->fl   = (float *) XtMalloc( sizeof(float) * acount ) ;
   fptr = MRI_FLOAT_PTR (mri_matrix);
   for( ii=0 ; ii < acount ; ii++ ){
      atr->fl[ii] = *fptr++;
   }
   RETURN(atr);
}

/* read integer values from string or file into int attribute */
static ATR_int *
Update_int_atr(char *aname, char *ivstring)
{
   ATR_int *atr ;
   MRI_IMAGE *mri_matrix = NULL;
   float *fptr;
   int nx, ny, nxy,ii, acount;

   ENTRY("Update_int_atr");
   if( !THD_filename_pure(aname) ){
     WARNING_message("Illegal atrint name %s",aname) ;
     RETURN(NULL) ;
   }

   atr = (ATR_int *)XtMalloc(sizeof(ATR_int)) ;
   atr->type = ATR_INT_TYPE ;
   atr->name = XtNewString( aname ) ;

   /* parse floats from string to attribute */
   /* try reading as float file or 1D: expression */
   mri_matrix = mri_read_1D(ivstring);  /* string could be file name or commandline string */
   if (mri_matrix == NULL)   {
      mri_matrix = mri_1D_fromstring(ivstring);
   }

   if (mri_matrix == NULL)   {
      WARNING_message("Error reading integer attribute file");
      RETURN(NULL);
   }

   /* number of floats in attribute */
   nx = mri_matrix->nx; ny = mri_matrix->ny; acount = nx*ny;
   atr->nin  = acount ;
   atr->in   = (int *) XtMalloc( sizeof(int) * acount ) ;
   fptr = MRI_FLOAT_PTR (mri_matrix);
   for( ii=0 ; ii < acount ; ii++ ){
      atr->in[ii] = *fptr++;
   }

   RETURN(atr);
}
