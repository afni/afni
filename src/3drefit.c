/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

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
           "a good idea to make sure the changes have been made correctly!\n\n"
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
    "\n"
    "  -xdel dimx      Makes the size of the voxel the given dimension,\n"
    "  -ydel dimy      for the given axis (x,y,z); dimensions in mm.\n"
    "  -zdel dimz   ** WARNING: if you change a voxel dimension, you will\n"
    "                  probably have to change the origin as well.\n"
    "\n"
    "  -TR time        Changes the TR time to a new value (see 'to3d -help').\n"
    "  -notoff         Removes the slice-dependent time-offsets.\n"
    "  -Torg ttt       Set the time origin of the dataset to value 'ttt'.\n"
    "                  (Time origins are set to 0 in to3d.)\n"
    "               ** WARNING: these 3 options apply only to 3D+time datasets.\n"
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
    "                  It can only be used to alter attributes that are NOT\n"
    "                  directly mapped into dataset internal structures, since\n"
    "                  those structures are mapped back into attribute values\n"
    "                  as the dataset is being written to disk.  If you want\n"
    "                  to change such an attribute, you have to use the\n"
    "                  corresponding 3drefit option directly.\n"
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
    "-copyaux auxset   Copies the 'auxiliary' data from dataset 'auxset'\n"
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
    "The following options allow you to modify VOLREG fields:\n"
    "  -vr_mat <val1> ... <val12>   Use these twelve values for VOLREG_MATVEC_index.\n"
    "  [-vr_mat_ind <index>]        Index of VOLREG_MATVEC_index field to be modified. Optional, default index is 0.\n"
    "                               Note: You can only modify one VOLREG_MATVEC_index at a time.\n"
    "  -vr_center_old <x> <y> <z>   Use these 3 values for VOLREG_CENTER_OLD.\n"
    "  -vr_center_base <x> <y> <z>  Use these 3 values for VOLREG_CENTER_BASE.\n"
    "\n"
   );

   printf("++ Last program update: 08 Jul 2005\n");

   exit(0) ;
}

#define ASET_NULL 1
#define ASET_SELF 2

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset , * aset = NULL ;
                      int aset_code = 0    ; /* 14 Dec 1999 */
   THD_dataxes      * daxes ;
   int new_stuff = 0 ;
   int new_orient = 0 ; char orient_code[4] ; int xxor,yyor,zzor ;
   int new_xorg   = 0 ; float xorg ; int cxorg=0, dxorg=0 , duporg=0 ;
   int new_yorg   = 0 ; float yorg ; int cyorg=0, dyorg=0 ;
   int new_zorg   = 0 ; float zorg ; int czorg=0, dzorg=0 ;
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

   /*-------------------------- help me if you can? --------------------------*/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax(NULL) ;

   iarg = 1 ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3drefit main"); machdep() ; PRINT_VERSION("3drefit") ;

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3drefit",argc,argv) ;

   while( iarg < argc && argv[iarg][0] == '-' ){

#if 0
      if( strncmp(argv[iarg],"-v",5) == 0 ){ verbose = 1 ; iarg++ ; continue ; }
#endif

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

        DSET_delete(qset) ; new_stuff++ ;

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

        new_stuff++ ;
       atrstring_done:
        iarg++ ; continue ;
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

   if( new_stuff == 0 ) Syntax("No options given!?") ;
   if( iarg >= argc   ) Syntax("No datasets given!?") ;

   if( new_orient && (dxorg || dyorg || dzorg) )     /* 02 Mar 2000 */
      Syntax("Can't use -orient with -d?origin!?") ;

   /*--- process datasets ---*/

   for( ; iarg < argc ; iarg++ ){
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
      if( DSET_IS_NIFTI(dset) ){
         ERROR_message("Can't process NIFTI dataset %s\n",argv[iarg]);
         continue ;
      }
      if( DSET_IS_MPEG(dset) ){
         ERROR_message("Can't process MPEG dataset %s\n",argv[iarg]);
         continue ;
      }
      INFO_message("Processing AFNI dataset %s\n",argv[iarg]) ;

      tross_Make_History( "3drefit" , argc,argv, dset ) ;

      /* 21 Dec 2004: -label2 option */

      if( new_label2 != NULL )
        EDIT_dset_items( dset , ADN_label2 , new_label2 , ADN_none ) ;

      /* 14 Oct 1999: change anat parent */
      /* 14 Dec 1999: allow special cases: SELF and NULL */

      if( aset != NULL ){
         EDIT_dset_items( dset , ADN_anat_parent , aset , ADN_none ) ;
      } else if( aset_code == ASET_SELF ){
         EDIT_dset_items( dset , ADN_anat_parent , dset , ADN_none ) ;
      } else if( aset_code == ASET_NULL ){
         EDIT_ZERO_ANATOMY_PARENT_ID( dset ) ;
         dset->anat_parent_name[0] = '\0' ;
      }

      /* Oct 04/02: zmodify volreg fields */
      if (Do_volreg_mat) {
         sprintf(str,"VOLREG_MATVEC_%06d", volreg_matind) ;
         INFO_message("Modifying %s ...\n", str);
         THD_set_float_atr( dset->dblk , str , 12 , volreg_mat ) ;
      }

      if (Do_center_old) {
         INFO_message("Modifying VOLREG_CENTER_OLD ...\n");
         THD_set_float_atr( dset->dblk , "VOLREG_CENTER_OLD" , 3 , center_old ) ;
      }

      if (Do_center_base) {
        INFO_message("Modifying VOLREG_CENTER_BASE ...\n");
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
        }
      }

      if( redo_bstat ){
        THD_load_statistics( dset ) ;   /* 01 Feb 2005 */
      }

      /* 25 April 1998 */

      if( new_byte_order > 0 )
         dset->dblk->diskptr->byte_order = new_byte_order ;

      daxes = dset->daxes ;

      if( new_orient ){
         daxes->xxorient = xxor ;
         daxes->yyorient = yyor ;
         daxes->zzorient = zzor ;
      }

      if( !new_xorg ) xorg = fabs(daxes->xxorg) ;
      if( !new_yorg ) yorg = fabs(daxes->yyorg) ;
      if( !new_zorg ) zorg = fabs(daxes->zzorg) ;

      if( !new_xdel ) xdel = fabs(daxes->xxdel) ;
      if( !new_ydel ) ydel = fabs(daxes->yydel) ;
      if( !new_zdel ) zdel = fabs(daxes->zzdel) ;

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

      if( new_TR ){
         if( dset->taxis == NULL ){
            WARNING_message("Can't process -TR for this dataset!\n") ;
         } else {
            float frac = TR / dset->taxis->ttdel ;
            int ii ;

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
          dset->taxis->ttorg = Torg ;
        }
      }

      if( new_toff_sl ){              /* 12 Feb 2001 */
         if( dset->taxis == NULL ){
            WARNING_message("-notoff: dataset has no time axis to clear!\n") ;
         } else if( dset->taxis->nsl <= 0 ){
            WARNING_message("-notoff: dataset has no time-offsets to clear!\n") ;
         } else {
            EDIT_dset_items( dset , ADN_nsl,0 , ADN_none ) ;
         }
      }

      if( (new_orient || new_zorg) && dset->taxis != NULL && dset->taxis->nsl > 0 ){
         dset->taxis->zorg_sl = daxes->zzorg ;
      }

      if( (new_orient || new_zdel) && dset->taxis != NULL && dset->taxis->nsl > 0 ){
         dset->taxis->dz_sl = daxes->zzdel ;
      }

      if( new_idcode ) dset->idcode = MCW_new_idcode() ;

      if( new_nowarp ){
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
            INFO_message("Changed dataset view type and filenames.\n") ;
         }
      }

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

      for( ii=0 ; ii < num_atrcopy ; ii++ )
        THD_insert_atr( dset->dblk , atrcopy[ii] ) ;

      if( denote ) THD_anonymize_write(1) ;   /* 08 Jul 2005 */

      THD_write_3dim_dataset( NULL,NULL , dset , False ) ;
      THD_delete_3dim_dataset( dset , False ) ;
   }
   exit(0) ;
}
