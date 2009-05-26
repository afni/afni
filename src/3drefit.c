/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

static ATR_float *Update_float_atr(char *aname, char *fvstring);
static ATR_int *Update_int_atr(char *aname, char *ivstring);

void Syntax(char *str)
{
   int ii ;

   if( str != NULL ) ERROR_exit(str) ;  /* does not return */

   printf( "Changes some of the information inside a 3D dataset's header.\n"
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
"Usage: 3drefit [options] dataset ...\n"
"where the options are\n"
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
"  -notoff         Removes the slice-dependent time-offsets.\n"
"  -Torg ttt       Set the time origin of the dataset to value 'ttt'.\n"
"                  (Time origins are set to 0 in to3d.)\n"
"               ** WARNING: These 3 options apply only to 3D+time datasets.\n"
"                   **N.B.: Using '-TR' on a dataset without a time axis\n"
"                           will add a time axis to the dataset.\n"
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
"\n"
"  -label2 llll    Set the 'label2' field in a dataset .HEAD file to the\n"
"                  string 'llll'.  (Can be used as in AFNI window titlebars.)\n"
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
    "  -atrfloat name 'values'\n"
    "  -atrint name 'values'\n"
    "                  Create or modify floating point or integer attributes.\n"
    "                  The input values may be specified as a single string\n"
    "                  in quotes or as a 1D filename or string. For example,\n"
    "     3drefit -atrfloat IJK_TO_DICOM_REAL '1 0 0 0 0 1 0 0 0 0 0 1'"
          " dset+orig\n"
    "     3drefit -atrfloat IJK_TO_DICOM_REAL flipZ.1D dset+orig\n"
    "     3drefit -atrfloat IJK_TO_DICOM_REAL '1D:1,3@0,0,1,2@0,2@0,1,0'"
          " dset+orig\n"
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
    "  -copyaux auxset Copies the 'auxiliary' data from dataset 'auxset'\n"
    "                  over the auxiliary data for the dataset being\n"
    "                  modified.  Auxiliary data comprises sub-brick labels,\n"
    "                  keywords, and statistics codes.\n"
    "                  '-copyaux' occurs BEFORE the '-sub' operations below,\n"
    "                  so you can use those to alter the auxiliary data\n"
    "                  that is copied from auxset.\n"
    "\n" ) ;

   printf(
    "The options below allow you to attach auxiliary data to sub-bricks\n"
    "in the dataset.  Each option may be used more than once so that\n"
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
   printf("         type  Description  PARAMETERS\n"
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
    "The following options allow you to modify VOLREG fields:\n"
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
    "The following options let you modify the FDR curves stored in the header:\n"
    "\n"
    " -addFDR = For each sub-brick marked with a statistical code, (re)compute\n"
    "           the FDR curve of z(q) vs. statistic, and store in the dataset header\n"
    "           * '-addFDR' runs as if '-new -pmask' were given to 3dFDR, so that\n"
    "              stat values == 0 will be ignored in the FDR algorithm.\n"
    "\n"
    " -FDRmask mset = load dataset 'mset' and use it as a mask\n"
    "                 for the '-addFDR' calculations.\n"
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

#define ASET_NULL 1
#define ASET_SELF 2

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset , * aset = NULL , *waset = NULL;
                      int aset_code = 0    ; /* 14 Dec 1999 */
                      int waset_code = 0;
   THD_dataxes      * daxes ;
   int new_stuff = 0 ;
   int new_orient = 0 ; char orient_code[4] ; int xxor,yyor,zzor ;
   int new_xorg   = 0 ; float xorg ; int cxorg=0, dxorg=0 , duporg=0 ;
   int new_yorg   = 0 ; float yorg ; int cyorg=0, dyorg=0 ;
   int new_zorg   = 0 ; float zorg ; int czorg=0, dzorg=0 ;
   int new_tags   = 0 ; int shift_tags = 0 ; /* 08 May 2006 [rickr] */
                        float dxtag=0.0, dytag=0.0, dztag=0.0 ;
   int new_xdel   = 0 ; float xdel ;
   int new_ydel   = 0 ; float ydel ;
   int new_zdel   = 0 ; float zdel ;
   int new_TR     = 0 ; float TR ;
   int new_Torg   = 0 ; float Torg ; /* 29 Jan 2003 */
   int new_tunits = 0 ; int tunits ;
   int new_idcode = 0 ;
   int new_nowarp = 0 ;
   int new_stataux= 0 ; float stataux[MAX_STAT_AUX] ;
   int new_type   = 0 ; int dtype , ftype , nvals ;
   int new_markers= 0 ;
   int new_view   = 0 ; int vtype ;
   int new_key    = 0 ; char * key ;
   int new_byte_order = 0 ;          /* 25 Apr 1998 */
   int new_toff_sl    = 0 ;          /* 12 Feb 2001 */
   int clear_bstat    = 0 ;          /* 28 May 2002 */
   int redo_bstat     = 0 ;          /* 01 Feb 2005 */
   int copyaux        = 0 ;          /* 08 Jun 2004 */
   THD_3dim_dataset *auxset=NULL ;   /* 08 Jun 2004 */
   char *new_label2   = NULL ;       /* 21 Dec 2004 */
   int denote         = 0 ;          /* 08 Jul 2005 */
   Boolean write_output ;            /* 20 Jun 2006 [rickr] */
   int keepcen        = 0 ;          /* 17 Jul 2006 [RWCox] */
   float xyzscale     = 0.0f ;       /* 17 Jul 2006 */
   int deoblique  = 0;               /* 20 Jun 2007 [drg] */
   int use_oblique_origin = 0;       /* 01 Dec 2008 */
   int do_FDR = 0 ;                  /* 23 Jan 2008 [RWCox] */
   int do_killSTAT = 0 ;             /* 24 Jan 2008 [RWCox] */
   byte *FDRmask = NULL ;            /* 27 Mar 2009 [RWcox] */
   int  nFDRmask = 0 ;
   int   ndone=0 ;                   /* 18 Jul 2006 */
   int   verb =0 ;
#define VINFO(x) if(verb)ININFO_message(x)

   char str[256] ;
   int  iarg , ii ;

   typedef struct { int iv ; char lab[32] ; }              SUBlabel   ;
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
   char *lcpt=NULL;

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

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax(NULL) ;

   iarg = 1 ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3drefit main"); machdep() ; PRINT_VERSION("3drefit") ; AUTHOR("RW Cox") ;

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3drefit",argc,argv) ;

   while( iarg < argc && argv[iarg][0] == '-' ){

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

      if( strcasecmp(argv[iarg],"-FDRmask") == 0 ){   /*-- 27 Mar 2009 --*/
        THD_3dim_dataset *fset ;
        if( iarg+1 >= argc ) Syntax("need 1 argument after -FDRmask!") ;
        if( nFDRmask > 0 )   Syntax("can't have two -FDRmask options!") ;
        fset = THD_open_dataset( argv[++iarg] ) ; CHECK_OPEN_ERROR(fset,argv[iarg]) ;
        DSET_load(fset)                         ; CHECK_LOAD_ERROR(fset) ;
        FDRmask = THD_makemask( fset , 0 , 1.0f,-1.0f ) ;
        if( FDRmask == NULL ) Syntax("Can't use -FDRmask dataset!") ;
        nFDRmask = DSET_NVOX(fset) ; DSET_delete(fset) ;
        ii = THD_countmask(nFDRmask,FDRmask) ;
        if( ii < 100 ){
          WARNING_message("-FDRmask has only %d nonzero voxels: ignoring",ii) ;
          free(FDRmask) ; FDRmask = NULL ; nFDRmask = 0 ;
        } else {
          INFO_message("-FDRmask has %d nonzero voxels (out of %d total)",ii,nFDRmask) ;
        }
        iarg++ ; continue ;
      }

      /*----- -atrcopy dd nn [03 Aug 2005] -----*/

      if( strcmp(argv[iarg],"-atrcopy") == 0 ){
        THD_3dim_dataset *qset ; ATR_any *atr ;

        if( iarg+2 >= argc ) Syntax("need 2 arguments after -atrcopy!") ;

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
        ATR_string *atr ; char *aname , *xx ;

        if( iarg+2 >= argc ) Syntax("need 2 arguments after -atrstring!") ;

        aname = argv[++iarg] ;
        if( !THD_filename_pure(aname) ){
          WARNING_message("Illegal -atrstring name %s",aname) ;
          iarg++ ; goto atrstring_done ;
        }
        xx  = argv[++iarg] ;
        atr = (ATR_string *)XtMalloc(sizeof(ATR_string)) ;

        atr->type = ATR_STRING_TYPE ;
        atr->name = XtNewString( aname ) ;
        atr->nch  = strlen(xx)+1 ; ;
        atr->ch   = (char *)XtMalloc( sizeof(char) * atr->nch ) ;
        memcpy( atr->ch , xx , sizeof(char) * atr->nch ) ;

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

        if( iarg+2 >= argc ) Syntax("need 2 arguments after -atrfloat!") ;
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

        if( iarg+2 >= argc ) Syntax("need 2 arguments after -atrint!") ;
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

         if( iarg+1 >= argc ) Syntax("need 1 argument after -copyaux!") ;

         if( auxset != NULL ) Syntax("Can't have more than one -copyaux option!") ;

         iarg++ ; copyaux = 1 ;
         if( strcmp(argv[iarg],"NULL") == 0 ){  /* special case */
            auxset = NULL ;
         } else {
            auxset = THD_open_one_dataset( argv[iarg] ) ;
            if( auxset == NULL ) Syntax("Can't open -copyaux dataset!") ;
         }

         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -apar aset [14 Oct 1999] -----*/

      if( strcmp(argv[iarg],"-apar")       == 0 ||
          strcmp(argv[iarg],"-anatparent") == 0 ||
          strcmp(argv[iarg],"-aset")       == 0    ){

         if( iarg+1 >= argc )
            Syntax("need 1 argument after -apar!") ;

         if( aset != NULL || aset_code != 0 )                 /* 13-14 Dec 1999 */
            Syntax("Can't have more than one -apar option!");

         iarg++ ;
         if( strcmp(argv[iarg],"NULL") == 0 ){    /* 14 Dec 1999: special cases */
            aset_code = ASET_NULL ;
         } else if( strcmp(argv[iarg],"SELF") == 0 ){
            aset_code = ASET_SELF ;
         } else {
            aset = THD_open_one_dataset( argv[iarg] ) ;
            if( aset == NULL )
               Syntax("Can't open -apar dataset!") ;
         }

         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -wpar wset [ZSS June 06] -----*/

      if( strcmp(argv[iarg],"-wpar")       == 0 ||
          strcmp(argv[iarg],"-warpparent") == 0 ||
          strcmp(argv[iarg],"-wset")       == 0    ){

         if( iarg+1 >= argc )
            Syntax("need 1 argument after -wpar!") ;

         if( waset != NULL || waset_code != 0 )
            Syntax("Can't have more than one -wpar option!");

         iarg++ ;
         if( strcmp(argv[iarg],"NULL") == 0 ){
            waset_code = ASET_NULL ;
         } else if( strcmp(argv[iarg],"SELF") == 0 ){
            waset_code = ASET_SELF ;
         } else {
            waset = THD_open_one_dataset( argv[iarg] ) ;
            if( waset == NULL )
               Syntax("Can't open -wpar dataset!") ;
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
            Syntax("need 1 argument after -byteorder!") ;

         iarg++ ;
         if( strcmp(argv[iarg],LSB_FIRST_STRING) == 0 )
            new_byte_order = LSB_FIRST ;
         else if( strcmp(argv[iarg],MSB_FIRST_STRING) == 0 )
            new_byte_order = MSB_FIRST ;
         else if( strcmp(argv[iarg],NATIVE_STRING) == 0 )
            new_byte_order = mri_short_order() ;
         else
            Syntax("illegal argument after -byteorder!") ;

         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -sublabel option -----*/

      if( strncmp(argv[iarg],"-sublabel",7) == 0 ){
         if( iarg+2 >= argc )
            Syntax("need 2 arguments after -sublabel!") ;

         iv = strtol( argv[++iarg] , &cpt , 10 ) ;
         if( iv < 0 || iv == 0 && cpt == argv[iarg] )
            Syntax("illegal sub-brick index after -sublabel!") ;

         sublab = (SUBlabel *) XtRealloc( (char *)sublab ,
                                          sizeof(SUBlabel) * (nsublab+1) ) ;

         sublab[nsublab].iv = iv ;
         MCW_strncpy( sublab[nsublab].lab , argv[++iarg] , 32 ) ;
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
            Syntax("need arguments after -sub...key!") ;

         iv = strtol( argv[++iarg] , &cpt , 10 ) ;
         if( iv < 0 || iv == 0 && cpt == argv[iarg] )
            Syntax("illegal sub-brick index after -sub...key!") ;

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
            Syntax("need arguments after -...key!") ;

         new_key = code ;
         if( code != 3 ) key = argv[++iarg] ;
         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /*----- -substatpar option -----*/

      if( strncmp(argv[iarg],"-substatpar",7) == 0 ){
         int fc ; float val ;

         if( iarg+2 >= argc )
            Syntax("need at least 2 arguments after -substatpar!") ;

         iv = strtol( argv[++iarg] , &cpt , 10 ) ;
         if( iv < 0 || iv == 0 && cpt == argv[iarg] )
            Syntax("illegal sub-brick index after -substatpar!") ;

         iarg++ ;
         if( strlen(argv[iarg]) < 3 )
            Syntax("illegal type code after -substatpar!") ;
         fc = (argv[iarg][0] == '-') ? 1 : 0 ;

         for( ii=FIRST_FUNC_TYPE ; ii <= LAST_FUNC_TYPE ; ii++ ){
            if( ! FUNC_IS_STAT(ii) ) continue ;
            if( strncmp( &(argv[iarg][fc]) ,
                         FUNC_prefixstr[ii] , THD_MAX_PREFIX ) == 0 ) break ;
         }

         if( ii > LAST_FUNC_TYPE )
            Syntax("unknown type code after -substatpar!") ;

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
         } while( iarg < argc ) ;

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

         if( iarg+1 >= argc ) Syntax("need an argument after -orient!");

         MCW_strncpy(orient_code,argv[++iarg],4) ;
         if( strlen(orient_code) != 3 ) Syntax("Illegal -orient code") ;

         acod = toupper(orient_code[0]) ; xxor = ORCODE(acod) ;
         acod = toupper(orient_code[1]) ; yyor = ORCODE(acod) ;
         acod = toupper(orient_code[2]) ; zzor = ORCODE(acod) ;

        if( xxor<0 || yyor<0 || zzor<0 || ! OR3OK(xxor,yyor,zzor) )
           Syntax("Unusable -orient code!") ;

         new_orient = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** -?origin dist **/

      if( strcmp(argv[iarg],"-xorigin") == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -xorigin!");
         if( strncmp(argv[iarg],"cen",3) == 0 ) cxorg = 1 ;
         else                                   xorg  = strtod(argv[iarg],NULL) ;
         dxorg = 0 ; new_xorg = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-yorigin") == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -yorigin!");
         if( strncmp(argv[iarg],"cen",3) == 0 ) cyorg = 1 ;
         else                                   yorg  = strtod(argv[iarg],NULL) ;
         dyorg = 0 ; new_yorg = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-zorigin") == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -zorigin!");
         if( strncmp(argv[iarg],"cen",3) == 0 ) czorg = 1 ;
         else                                   zorg  = strtod(argv[iarg],NULL) ;
         dzorg = 0 ; new_zorg = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /* 13 Sep 2000: -duporigin */

      if( strcmp(argv[iarg],"-duporigin") == 0 ){
         THD_3dim_dataset * cset ;
         if( ++iarg >= argc ) Syntax("need an argument after -duporigin!");
         cset = THD_open_dataset( argv[iarg] ) ;
         if( cset == NULL ) Syntax("couldn't open -duporigin dataset!");
         daxes = cset->daxes ;
         xorg = daxes->xxorg ; yorg = daxes->yyorg ; zorg = daxes->zzorg ;
         cxorg = cyorg = czorg = dxorg = dyorg = dzorg = 0 ;
         new_xorg = new_yorg = new_zorg = duporg = 1 ; new_stuff++ ;
         DSET_delete(cset) ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /* 02 Mar 2000: -d?origin stuff, to go with plug_nudge.c */

      if( strncmp(argv[iarg],"-dxorigin",4) == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -dxorigin!");
         xorg = strtod(argv[iarg],NULL) ; dxorg = 1 ; cxorg = 0 ;
         new_xorg = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-dyorigin",4) == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -dyorigin!");
         yorg = strtod(argv[iarg],NULL) ; dyorg = 1 ; cyorg = 0 ;
         new_yorg = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-dzorigin",4) == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -dzorigin!");
         zorg = strtod(argv[iarg],NULL) ; dzorg = 1 ; czorg = 0 ;
         new_zorg = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** 04 Oct 2002: _raw origins **/

      if( strcmp(argv[iarg],"-xorigin_raw") == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -xorigin_raw!");
         xorg     = strtod(argv[iarg],NULL) ; cxorg = dxorg = 0 ;
         new_xorg = 2 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-yorigin_raw") == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -yorigin_raw!");
         yorg     = strtod(argv[iarg],NULL) ; cyorg = dyorg = 0 ;
         new_yorg = 2 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-zorigin_raw") == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -zorigin_raw!");
         zorg     = strtod(argv[iarg],NULL) ; czorg = dzorg = 0 ;
         new_zorg = 2 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** 04 Oct 2002: zadd VOLREG fields **/
      if( strcmp(argv[iarg],"-vr_mat") == 0 ){
         if( iarg+12 >= argc ) Syntax("need 12 arguments after -vr_mat!");
         icnt = 0;
         while (icnt < 12) {
            ++iarg;
            volreg_mat[icnt] = strtod(argv[iarg], &lcpt) ; if (*lcpt != '\0') Syntax("Bad syntax in list of numbers!");
            ++icnt;
         }
         Do_volreg_mat = 1; new_stuff++ ;
         ++iarg;
         continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-vr_mat_ind") == 0) {
         if (++iarg >= argc) Syntax("need 1 argument after -vr_mat_ind!");
         volreg_matind = (int)strtol(argv[iarg], &lcpt, 10); if (*lcpt != '\0') Syntax("Bad syntax in number argument!");
         ++iarg;
         continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-vr_cen_old") == 0) {
         if (iarg+3 >= argc) Syntax("need 3 arguments after -vr_cen_old");
         ++iarg;
         center_old[0] = strtod(argv[iarg],&lcpt) ; ++iarg; if (*lcpt != '\0') Syntax("Bad syntax in list of numbers!");
         center_old[1] = strtod(argv[iarg],&lcpt) ; ++iarg; if (*lcpt != '\0') Syntax("Bad syntax in list of numbers!");
         center_old[2] = strtod(argv[iarg],&lcpt) ;  if (*lcpt != '\0') Syntax("Bad syntax in list of numbers!");
         Do_center_old = 1; new_stuff++ ;
         ++iarg;
         continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-vr_cen_base") == 0) {
         if (iarg+3 >= argc) Syntax("need 3 arguments after -vr_cen_base");
         ++iarg;
         center_base[0] = strtod(argv[iarg],&lcpt) ; ++iarg; if (*lcpt != '\0') Syntax("Bad syntax in list of numbers!");
         center_base[1] = strtod(argv[iarg],&lcpt) ; ++iarg; if (*lcpt != '\0') Syntax("Bad syntax in list of numbers!");
         center_base[2] = strtod(argv[iarg],&lcpt) ;  if (*lcpt != '\0') Syntax("Bad syntax in list of numbers!");
         Do_center_base = 1; new_stuff++ ;
         ++iarg;
         continue ;  /* go to next arg */
      }

      /** -?del dim **/

      if( strncmp(argv[iarg],"-xdel",4) == 0 ){
         if( iarg+1 >= argc ) Syntax("need an argument after -xdel!");
         xdel = strtod( argv[++iarg]  , NULL ) ;
         if( xdel <= 0.0 ) Syntax("argument after -xdel must be positive!") ;
         new_xdel = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-ydel",4) == 0 ){
         if( iarg+1 >= argc ) Syntax("need an argument after -ydel!");
         ydel = strtod( argv[++iarg]  , NULL ) ;
         if( ydel <= 0.0 ) Syntax("argument after -ydel must be positive!") ;
         new_ydel = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-zdel",4) == 0 ){
         if( iarg+1 >= argc ) Syntax("need an argument after -zdel!");
         zdel = strtod( argv[++iarg]  , NULL ) ;
         if( zdel <= 0.0 ) Syntax("argument after -zdel must be positive!") ;
         new_zdel = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-keepcen",7) == 0 ){  /* 17 Jul 2006 */
        keepcen = 1 ;
        iarg++ ; continue ;  /* go to next arg */
      }

      if( strcmp(argv[iarg],"-verb") == 0 ){
        verb++ ; iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-xyzscale",8) == 0 ){ /* 17 Jul 2006 */
         if( iarg+1 >= argc ) Syntax("need an argument after -xyzscale!");
         xyzscale = strtod( argv[++iarg] , NULL ) ;
         if( xyzscale <= 0.0f ) Syntax("argument after -xyzscale must be positive!");
         if( xyzscale == 1.0f )
           WARNING_message(
            "-xyzscale 1.0 really makes no sense, but if that's what you want" ) ;
         new_stuff++ ; iarg++ ; continue ;  /* go to next arg */
      }

      /** -TR **/

      if( strncmp(argv[iarg],"-TR",3) == 0 ){
         char * eptr ;
         if( iarg+1 >= argc ) Syntax("need an argument after -TR!");
         TR = strtod( argv[++iarg]  , &eptr ) ;
         if( TR <= 0.0 ) Syntax("argument after -TR must be positive!") ;

         if( strcmp(eptr,"ms")==0 || strcmp(eptr,"msec")==0 ){
            new_tunits = 1 ; tunits = UNITS_MSEC_TYPE ;
            WARNING_message("TR expressed in milliseconds is deprecated.") ;
         } else if( strcmp(eptr,"s")==0 || strcmp(eptr,"sec")==0 ){
            new_tunits = 1 ; tunits = UNITS_SEC_TYPE ;
         } else if( strcmp(eptr,"Hz")==0 || strcmp(eptr,"Hertz")==0 ){
            new_tunits = 1 ; tunits = UNITS_HZ_TYPE ;
         }

         new_TR = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** -notoff (12 Feb 2001) **/

      if( strncmp(argv[iarg],"-notoff",7) == 0 ){
         new_toff_sl = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      /** -Torg (29 Jan 2003) **/

      if( strncmp(argv[iarg],"-Torg",5) == 0 ){
        char *eptr ;
        if( iarg+1 >= argc ) Syntax("need an argument after -Torg!");
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

         if( ++iarg >= argc ) Syntax("need an argument after -statpar!") ;

         for( ii=0 ; ii < MAX_STAT_AUX ; ii++ ) stataux[ii] = 0.0 ;

         ii = 0 ;
         do{
            val = strtod( argv[iarg] , &ptr ) ;
            if( *ptr != '\0' ) break ;
            stataux[ii++] = val ;
            iarg++ ;
         } while( iarg < argc ) ;

         if( ii == 0 ) Syntax("No numbers given after -statpar?") ;

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

      /** -view code **/

      if( strncmp(argv[iarg],"-view",4) == 0 ){
         char * code ;
         if( iarg+1 >= argc ) Syntax("need an argument after -view!") ;
         code = argv[++iarg] ; if( code[0] == '+' ) code++ ;
         for( vtype=0 ; vtype <= LAST_VIEW_TYPE ; vtype++ )
            if( strcmp(code,VIEW_codestr[vtype]) == 0 ) break ;
         if( vtype > LAST_VIEW_TYPE ) Syntax("argument after -view is illegal!") ;
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
         if( ++iarg >= argc ) Syntax("need an argument after -dxtag!");
         dxtag = strtod(argv[iarg],NULL) ;
         new_tags = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-dytag",6) == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -dytag!");
         dytag = strtod(argv[iarg],NULL) ;
         new_tags = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-dztag",6) == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -dztag!");
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


      /** anything else must be a -type **/
      /*  try the anatomy prefixes */

      for( ii=FIRST_ANAT_TYPE ; ii <= LAST_ANAT_TYPE ; ii++ )
         if( strncmp( &(argv[iarg][1]) ,
                      ANAT_prefixstr[ii] , THD_MAX_PREFIX ) == 0 ) break ;

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
        sprintf(str,"Unknown option %s",argv[iarg]) ;
        Syntax(str) ;
      }

   }  /* end of loop over switches */

   /*-- some checks for erroneous inputs --*/

   if( new_stuff == 0 && atrmod == 0 ) Syntax("No options given!?") ;
   if( new_stuff == 1 && atrmod == 1 ){       /* 28 Jul 2006 [rickr] */
      fprintf(stderr,"** Cannot use -atrcopy or -atrstring with other "
                     "modification options.\n");
      Syntax("Illegal attribute syntax.");
   }
   if( iarg >= argc   ) Syntax("No datasets given!?") ;

   if( xyzscale != 0.0f &&
       (new_orient || new_xorg || new_yorg || new_zorg ||
        keepcen    || new_xdel || new_ydel || new_zdel   ) ){  /* 18 Jul 2006 */
    Syntax(
    "-xyzscale is incompatible with other options for changing voxel grid");
   }

   if( new_orient && (dxorg || dyorg || dzorg) )     /* 02 Mar 2000 */
      Syntax("Can't use -orient with -d?origin!?") ;

   if( new_tags || shift_tags ){                     /* 08 May 2006 [rickr] */
      if( new_tags && shift_tags )
         Syntax("Cant' use -shift_tags with -d{xyz}tag") ;
      if( new_orient )
         Syntax("Can't use -orient with -shift_tags or -d{xyz}tags") ;
      if( shift_tags && !dxorg && !dyorg && !dzorg )
         Syntax("-shift_tags option requires a -d{xyz}origin option") ;

      if( shift_tags ){    /* then copy shifts to tag vars */
         if( dxorg ) dxtag = xorg ;
         if( dyorg ) dytag = yorg ;
         if( dzorg ) dztag = zorg ;
      }
   }

   /*--- process datasets ---*/
   for( ; iarg < argc ; iarg++ ){
      write_output = False ;   /* some datasets will be overwritten */

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

      tross_Make_History( "3drefit" , argc,argv, dset ) ;

      /* 21 Dec 2004: -label2 option */

      if( new_label2 != NULL ){
        EDIT_dset_items( dset , ADN_label2 , new_label2 , ADN_none ) ;
        VINFO("setting label2") ;
      }

      /* 14 Oct 1999: change anat parent */
      /* 14 Dec 1999: allow special cases: SELF and NULL */

      if( aset != NULL ){
         EDIT_dset_items( dset , ADN_anat_parent , aset , ADN_none ) ;
         VINFO("setting Anat parent") ;
      } else if( aset_code == ASET_SELF ){
         EDIT_dset_items( dset , ADN_anat_parent , dset , ADN_none ) ;
         VINFO("setting Anat parent") ;
      } else if( aset_code == ASET_NULL ){
         EDIT_ZERO_ANATOMY_PARENT_ID( dset ) ;
         dset->anat_parent_name[0] = '\0' ;
         VINFO("clearing Anat parent") ;
      }

      /* ZSS June 06, add a warp parent field please */
      if( waset != NULL ){
         EDIT_dset_items( dset , ADN_warp_parent , waset , ADN_none ) ;
         VINFO("setting Warp parent") ;
      } else if( waset_code == ASET_SELF ){
         EDIT_dset_items( dset , ADN_warp_parent , dset , ADN_none ) ;
         VINFO("setting Warp parent") ;
      } else if( waset_code == ASET_NULL ){
         EDIT_ZERO_ANATOMY_PARENT_ID( dset ) ;
         dset->warp_parent_name[0] = '\0' ;
         VINFO("clearing Warp parent") ;
      }
      /* Oct 04/02: zmodify volreg fields */
      if (Do_volreg_mat) {
         sprintf(str,"VOLREG_MATVEC_%06d", volreg_matind) ;
         if( verb ) ININFO_message("Modifying %s ...\n", str);
         THD_set_float_atr( dset->dblk , str , 12 , volreg_mat ) ;
      }

      if (Do_center_old) {
         VINFO("Modifying VOLREG_CENTER_OLD ...\n");
         THD_set_float_atr( dset->dblk , "VOLREG_CENTER_OLD" , 3 , center_old ) ;
      }

      if (Do_center_base) {
        VINFO("Modifying VOLREG_CENTER_BASE ...\n");
        THD_set_float_atr( dset->dblk , "VOLREG_CENTER_BASE" , 3 , center_base ) ;
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
        }
      }

      if( redo_bstat ){
        VINFO("reloading brick statistics") ;
        THD_load_statistics( dset ) ;   /* 01 Feb 2005 */
      }

      if( new_byte_order > 0 ){
         VINFO("changing byte order") ;
         dset->dblk->diskptr->byte_order = new_byte_order ; /* 25 April 1998 */
      }

      /*-- change space axes (lots of possibilities here) --*/

      daxes = dset->daxes ;

      if( new_orient ){
         VINFO("changing orientation codes") ;
         daxes->xxorient = xxor ;
         daxes->yyorient = yyor ;
         daxes->zzorient = zzor ;
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
         daxes->xxorg += xorg ;
      else if( duporg || new_xorg==2 )
         daxes->xxorg = xorg ;
      else if( new_xorg==1 || new_orient )
         daxes->xxorg = (ORIENT_sign[daxes->xxorient] == '+') ? (-xorg) : (xorg) ;

      if( dyorg )
         daxes->yyorg += yorg ;
      else if( duporg || new_yorg==2 )
         daxes->yyorg = yorg ;
      else if( new_yorg==1 || new_orient )
         daxes->yyorg = (ORIENT_sign[daxes->yyorient] == '+') ? (-yorg) : (yorg) ;

      if( dzorg )
         daxes->zzorg += zorg ;
      else if( duporg || new_zorg==2 )
         daxes->zzorg = zorg ;
      else if( new_zorg==1 || new_orient )
         daxes->zzorg = (ORIENT_sign[daxes->zzorient] == '+') ? (-zorg) : (zorg) ;

      if( new_xdel || new_orient )
         daxes->xxdel = (ORIENT_sign[daxes->xxorient] == '+') ? (xdel) : (-xdel) ;

      if( new_ydel || new_orient )
         daxes->yydel = (ORIENT_sign[daxes->yyorient] == '+') ? (ydel) : (-ydel) ;

      if( new_zdel || new_orient )
         daxes->zzdel = (ORIENT_sign[daxes->zzorient] == '+') ? (zdel) : (-zdel) ;

      /*-- deoblique - assume the data is cardinal  6/20/2007 */
      /* this should be after any other axis, orientation, origin, voxel size changes */
      if(deoblique) {
         /* replace transformation matrix with cardinal form */
	 THD_dicom_card_xform(dset, &tmat, &tvec);
	 LOAD_MAT44(dset->daxes->ijk_to_dicom_real,
             tmat.mat[0][0], tmat.mat[0][1], tmat.mat[0][2], tvec.xyz[0],
             tmat.mat[1][0], tmat.mat[1][1], tmat.mat[1][2], tvec.xyz[1],
             tmat.mat[2][0], tmat.mat[2][1], tmat.mat[2][2], tvec.xyz[2]);
      }


      /* if user has selected, get origin from obliquity */
      /*   overriding all the previous command-line options */
      if(use_oblique_origin)
         Obliquity_to_coords(dset);


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
         }
      }

      if( new_Torg ){                   /* 29 Jan 2003 */
        if( dset->taxis == NULL ){
          WARNING_message("Can't process -Torg for this dataset!\n") ;
        } else {
          VINFO("changing Torg") ;
          dset->taxis->ttorg = Torg ;
        }
      }

      if( new_toff_sl ){              /* 12 Feb 2001 */
         if( dset->taxis == NULL ){
            WARNING_message("-notoff: dataset has no time axis to clear!\n") ;
         } else if( dset->taxis->nsl <= 0 ){
            WARNING_message("-notoff: dataset has no time-offsets to clear!\n") ;
         } else {
            VINFO("clearing time-offsets") ;
            EDIT_dset_items( dset , ADN_nsl,0 , ADN_none ) ;
         }
      }

      if( (new_orient || new_zorg) && dset->taxis != NULL && dset->taxis->nsl > 0 ){
         VINFO("changing time axis slice offset z-origin") ;
         dset->taxis->zorg_sl = daxes->zzorg ;
      }

      if( (new_orient || new_zdel) && dset->taxis != NULL && dset->taxis->nsl > 0 ){
         VINFO("changing time axis slice offset z-spacing") ;
         dset->taxis->dz_sl = daxes->zzdel ;
      }

      if( new_idcode ){
        VINFO("changing ID code") ;
        dset->idcode = MCW_new_idcode() ;
      }

      if( new_nowarp ){
         VINFO("clearing warp") ;
         ZERO_IDCODE( dset->warp_parent_idcode ) ;
         dset->warp_parent_name[0] = '\0' ;
         dset->warp = NULL ;
      }

      if( new_type ){
         if( nvals > 1 && dset->taxis != NULL ){
            ERROR_message("Can't change 3D+time dataset to new type:\n"
                          " *    new type has more than one value per voxel!\n") ;
         } else if( dset->taxis == NULL && nvals != dset->dblk->nvals &&
                    ((dtype==HEAD_FUNC_TYPE && ftype!=FUNC_BUCK_TYPE)||
                     (dtype==HEAD_ANAT_TYPE && ftype!=ANAT_BUCK_TYPE)  ) ){

            ERROR_message("Can't change dataset to new type:\n"
                          " *     mismatch in number of sub-bricks!\n") ;
         } else {
            VINFO("changing dataset 'type' marker") ;
            dset->type      = dtype ;
            dset->func_type = ftype ;

            if( ISBUCKET(dset) && dset->taxis != NULL ){   /* 29 April 1998 */
              WARNING_message("changing 3D+time dataset to bucket\n") ;
              EDIT_dset_items( dset , ADN_ntt , 0 , ADN_none ) ;
            }

         }
      }

      if( new_stataux ){
         for( ii=0 ; ii < MAX_STAT_AUX ; ii++ ){
            dset->stat_aux[ii] = stataux[ii] ;
         }
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

         strcpy(new_head,DSET_DIRNAME(dset)) ; strcat(new_head,DSET_HEADNAME(dset)) ;
         strcpy(new_brik,DSET_DIRNAME(dset)) ; strcat(new_brik,DSET_BRIKNAME(dset)) ;

         if( THD_is_file(new_head) ){
            dset->view_type = old_vtype ;
            THD_init_diskptr_names( dset->dblk->diskptr ,
                                    NULL , NULL , NULL , old_vtype , True ) ;
            ERROR_message("Can't change view: would overwrite existing files!\n") ;
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
         }
      } else if ( dset->tagset && ( new_xorg || new_yorg || new_zorg ||
                                    new_xdel || new_ydel || new_zdel ) )
         WARNING_message("modifying coordinates of dataset with tags") ;

      /* code moved to edt_emptycopy.c                   13 Sep 2005 [rickr] */
      if( new_markers && okay_to_add_markers(dset) ){
         dset->markers = create_empty_marker_set() ;

      } else if( new_markers ){
         WARNING_message("Can't add markers to this dataset\n") ;
      } /* end of markers */

      /*-- 08 Jun 2004: copyaux? --*/

      if( copyaux ){
        if( auxset != NULL ){
          THD_copy_datablock_auxdata( auxset->dblk , dset->dblk );
          INIT_STAT_AUX( dset , MAX_STAT_AUX , auxset->stat_aux ) ;
        } else {
          THD_copy_datablock_auxdata( NULL , dset->dblk );
        }
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
            }
         }
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
            } else if( code == 2 ){
               EDIT_dset_items( dset ,
                                   ADN_brick_keywords_replace_one + iv ,
                                   subkeyword[ii].keyword ,
                                ADN_none ) ;
            } else if( code == 3 && dset->dblk->brick_keywords != NULL ){
               EDIT_dset_items( dset ,
                                   ADN_brick_keywords_replace_one + iv ,
                                   NULL ,
                                ADN_none ) ;
            }
         }
      }

      switch( new_key ){
        case 1: EDIT_dset_items(dset, ADN_keywords_append , key , ADN_none); break;
        case 2: EDIT_dset_items(dset, ADN_keywords_replace, key , ADN_none); break;
        case 3: EDIT_dset_items(dset, ADN_keywords_replace, NULL, ADN_none); break;
      }

      if( do_killSTAT ){   /* 24 Jan 2008 */
        for( iv=0 ; iv < DSET_NVALS(dset) ; iv++ ){
          EDIT_BRICK_TO_NOSTAT(dset,iv) ;
        }
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
          }
        }
      }

      /* 03 Aug 2005: implement atrcopy */
      {
           ATR_any *atr;
         for( ii=0 ; ii < num_atrcopy ; ii++ ) {
           THD_insert_atr( dset->dblk , atrcopy[ii] ) ;
         }
      }
      /* 23 Jan 2008: the FDR stuff */

      if( do_FDR ){
        DSET_BRICK_FDRCURVE_ALLKILL(dset) ;
        DSET_BRICK_MDFCURVE_ALLKILL(dset) ;  /* 22 Oct 2008 */
        if( do_FDR > 0 ){
          int nf ;
          mri_fdr_setmask( (nFDRmask == DSET_NVOX(dset)) ? FDRmask : NULL ) ;
          nf = THD_create_all_fdrcurves(dset) ;
          ININFO_message("created %d FDR curves in dataset header",nf) ;
        }
      }

      /* Do we want to force new attributes into output ? ZSS Jun 06*/
      /* (only if -atrcopy or -atrstring)       28 Jul 2006 [rickr] */
      if ( saveatr && atrmod ){
         /* apply attributes to header - dataxes and dblk*/
INFO_message("applying attributes");
         THD_datablock_from_atr(dset->dblk , DSET_DIRNAME(dset) ,
                                  dset->dblk->diskptr->header_name);
         THD_datablock_apply_atr(dset );
      }

      if( denote ) THD_anonymize_write(1) ;   /* 08 Jul 2005 */

      if( write_output ) DSET_load(dset) ;    /* 20 Jun 2006 */

      THD_force_ok_overwrite(1) ;             /* 24 Sep 2007 */
      THD_write_3dim_dataset( NULL,NULL , dset , write_output ) ;
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
