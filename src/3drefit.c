/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

void Syntax(char * str)
{
   int ii ;

   if( str != NULL ){
      printf("\n*** Fatal error: %s\n\n*** Try '3drefit -help'\n",str) ;
      exit(1) ;
   }

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
    "  -xdel dimx      Makes the size of the voxel the given dimension,\n"
    "  -ydel dimy      for the given axis (x,y,z); dimensions in mm.\n"
    "  -zdel dimz   ** WARNING: if you change a voxel dimension, you will\n"
    "                  probably have to change the origin as well.\n"
    "\n"
    "  -TR time        Changes the TR time to a new value (see 'to3d -help').\n"
    "               ** WARNING: this only applies to 3D+time datasets.\n"
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
    "  -type           Changes the type of data that is declared for this\n"
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

   exit(0) ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset , * aset = NULL ;
   THD_dataxes      * daxes ;
   int new_stuff = 0 ;
   int new_orient = 0 ; char orient_code[4] ; int xxor,yyor,zzor ;
   int new_xorg   = 0 ; float xorg ; int cxorg=0 ;
   int new_yorg   = 0 ; float yorg ; int cyorg=0 ;
   int new_zorg   = 0 ; float zorg ; int czorg=0 ;
   int new_xdel   = 0 ; float xdel ;
   int new_ydel   = 0 ; float ydel ;
   int new_zdel   = 0 ; float zdel ;
   int new_TR     = 0 ; float TR ;
   int new_tunits = 0 ; int tunits ;
   int new_idcode = 0 ;
   int new_nowarp = 0 ;
   int new_stataux= 0 ; float stataux[MAX_STAT_AUX] ;
   int new_type   = 0 ; int dtype , ftype , nvals ;
   int new_markers= 0 ;
   int new_view   = 0 ; int vtype ;
   int new_key    = 0 ; char * key ;
   int new_byte_order = 0 ;          /* 25 April 1998 */
   char str[256] ;
   int  iarg , ii ;

   typedef struct { int iv ; char lab[32] ; }              SUBlabel   ;
   typedef struct { int iv ; float par[MAX_STAT_AUX+2] ; } SUBstatpar ;
   typedef struct { int iv , code ; char * keyword ; }     SUBkeyword ;
   int nsublab     = 0 ; SUBlabel *   sublab     = NULL ;
   int nsubstatpar = 0 ; SUBstatpar * substatpar = NULL ;
   int nsubkeyword = 0 ; SUBkeyword * subkeyword = NULL ;
   char * cpt ;
   int iv ;

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax(NULL) ;

   iarg = 1 ;

   while( iarg < argc && argv[iarg][0] == '-' ){

#if 0
      if( strcmp(argv[iarg],"-v") == 0 ){ verbose = 1 ; iarg++ ; continue ; }
#endif

      /*----- -apar aset [14 Oct 1999] -----*/

      if( strcmp(argv[iarg],"-apar")       == 0 ||
          strcmp(argv[iarg],"-anatparent") == 0 ||
          strcmp(argv[iarg],"-aset")       == 0    ){

         if( iarg+1 >= argc )
            Syntax("need 1 argument after -apar!") ;

         if( aset != NULL )                                  /* 13 Dec 1999 */
            Syntax("can't have more than one -apar option!");

         iarg++ ;
         aset = THD_open_one_dataset( argv[iarg] ) ;
         if( aset == NULL )
            Syntax("can't open -apar dataset!") ;

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

      if( strncmp(argv[iarg],"-xorigin",4) == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -xorigin!");
         if( strncmp(argv[iarg],"cen",3) == 0 ) cxorg = 1 ;
         else                                   xorg  = strtod(argv[iarg],NULL) ;
         new_xorg = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-yorigin",4) == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -yorigin!");
         if( strncmp(argv[iarg],"cen",3) == 0 ) cyorg = 1 ;
         else                                   yorg  = strtod(argv[iarg],NULL) ;
         new_yorg = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[iarg],"-zorigin",4) == 0 ){
         if( ++iarg >= argc ) Syntax("need an argument after -zorigin!");
         if( strncmp(argv[iarg],"cen",3) == 0 ) czorg = 1 ;
         else                                   zorg  = strtod(argv[iarg],NULL) ;
         new_zorg = 1 ; new_stuff++ ;
         iarg++ ; continue ;  /* go to next arg */
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
         } else if( strcmp(eptr,"s")==0 || strcmp(eptr,"sec")==0 ){
            new_tunits = 1 ; tunits = UNITS_SEC_TYPE ;
         } else if( strcmp(eptr,"Hz")==0 || strcmp(eptr,"Hertz")==0 ){
            new_tunits = 1 ; tunits = UNITS_HZ_TYPE ;
         }

         new_TR = 1 ; new_stuff++ ;
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

   /*--- process datasets ---*/

   for( ; iarg < argc ; iarg++ ){
      dset = THD_open_one_dataset( argv[iarg] ) ;
      if( dset == NULL ){
         fprintf(stderr,"** Can't open dataset %s\n",argv[iarg]) ;
         continue ;
      }
      fprintf(stderr,"Processing dataset %s\n",argv[iarg]) ;

      tross_Make_History( "3drefit" , argc,argv, dset ) ;

      /* 14 Oct 1999 */

      if( aset != NULL )
         EDIT_dset_items( dset , ADN_anat_parent , aset , ADN_none ) ;

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

      if( new_xorg || new_orient )
         daxes->xxorg = (ORIENT_sign[daxes->xxorient] == '+') ? (-xorg) : (xorg) ;

      if( new_yorg || new_orient )
         daxes->yyorg = (ORIENT_sign[daxes->yyorient] == '+') ? (-yorg) : (yorg) ;

      if( new_zorg || new_orient )
         daxes->zzorg = (ORIENT_sign[daxes->zzorient] == '+') ? (-zorg) : (zorg) ;

      if( new_xdel || new_orient )
         daxes->xxdel = (ORIENT_sign[daxes->xxorient] == '+') ? (xdel) : (-xdel) ;

      if( new_ydel || new_orient )
         daxes->yydel = (ORIENT_sign[daxes->yyorient] == '+') ? (ydel) : (-ydel) ;

      if( new_zdel || new_orient )
         daxes->zzdel = (ORIENT_sign[daxes->zzorient] == '+') ? (zdel) : (-zdel) ;

      if( new_TR ){
         if( dset->taxis == NULL ){
            fprintf(stderr,"  ** can't process -TR for this dataset!\n") ;
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
            fprintf(stderr,"  ** can't change 3D+time dataset to new type:\n") ;
            fprintf(stderr,"     new type has more than one value per voxel!\n") ;
         } else if( dset->taxis == NULL && nvals != dset->dblk->nvals &&
                    ((dtype==HEAD_FUNC_TYPE && ftype!=FUNC_BUCK_TYPE)||
                     (dtype==HEAD_ANAT_TYPE && ftype!=ANAT_BUCK_TYPE)  ) ){

            fprintf(stderr,"  ** can't change dataset to new type:\n") ;
            fprintf(stderr,"     mismatch in number of sub-bricks!\n") ;
         } else {
            dset->type      = dtype ;
            dset->func_type = ftype ;

            if( ISBUCKET(dset) && dset->taxis != NULL ){   /* 29 April 1998 */
               fprintf(stderr,"  ** Warning: changing 3D+time dataset to bucket\n") ;
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
            fprintf(stderr,
                    "  ** Can't change view: would overwrite existing files!\n") ;
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
            fprintf(stderr,"  -- Changed dataset view type and filenames.\n") ;
         }
      }

      if( new_markers                           &&
          dset->type      == HEAD_ANAT_TYPE     &&
          dset->view_type == VIEW_ORIGINAL_TYPE &&
          DSET_NUM_TIMES(dset) == 1                ){  /* code copied from to3d.c */

         THD_marker_set * markers ;
         int ii , jj ;

         markers = dset->markers = myXtNew( THD_marker_set ) ;
         markers->numdef = 0 ;

         for( ii=0 ; ii < MARKS_MAXNUM ; ii++ ){       /* null all data out */
            markers->valid[ii] = 0 ;
            for( jj=0 ; jj < MARKS_MAXLAB  ; jj++ )
               markers->label[ii][jj] = '\0';
            for( jj=0 ; jj < MARKS_MAXHELP ; jj++ )
               markers->help[ii][jj]  = '\0';
         }

         for( ii=0 ; ii < NMARK_ALIGN ; ii++ ){       /* copy strings in */
            MCW_strncpy( &(markers->label[ii][0]) ,
                         THD_align_label[ii] , MARKS_MAXLAB ) ;
            MCW_strncpy( &(markers->help[ii][0]) ,
                         THD_align_help[ii] , MARKS_MAXHELP ) ;
         }

         for( ii=0 ; ii < MARKS_MAXFLAG ; ii++ )     /* copy flags in */
            markers->aflags[ii] = THD_align_aflags[ii] ;

      } else if( new_markers ){
            fprintf(stderr,"  ** can't add markers to this dataset\n") ;
      } /* end of markers */

      if( nsublab > 0 ){
         for( ii=0 ; ii < nsublab ; ii++ ){
            iv = sublab[ii].iv ;
            if( iv < 0 || iv >= DSET_NVALS(dset) ){
               fprintf(stderr,"  ** can't put label on sub-brick %d\n",iv) ;
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
               fprintf(stderr,"  ** can't put keyword on sub-brick %d\n",iv) ;
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
               fprintf(stderr,"  ** can't put statpar on sub-brick %d\n",iv) ;
            } else {
               EDIT_dset_items( dset ,
                                   ADN_brick_stataux_one + iv , substatpar[ii].par ,
                                ADN_none ) ;
            }
         }
      }

      THD_write_3dim_dataset( NULL,NULL , dset , False ) ;
      THD_delete_3dim_dataset( dset , False ) ;
   }
   exit(0) ;
}
