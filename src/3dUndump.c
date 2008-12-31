/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*-- these macros stolen from file thd.h --*/

#define ORCODE(aa) \
  ( (aa)=='R' ? ORI_R2L_TYPE : (aa)=='L' ? ORI_L2R_TYPE : \
    (aa)=='P' ? ORI_P2A_TYPE : (aa)=='A' ? ORI_A2P_TYPE : \
    (aa)=='I' ? ORI_I2S_TYPE : (aa)=='S' ? ORI_S2I_TYPE : ILLEGAL_TYPE )

#define OR3OK(x,y,z) ( ((x)&6) + ((y)&6) + ((z)&6) == 6 )

/*------------------------------------------------------------------------*/

void Syntax(void)
{
   printf(
    "Usage: 3dUndump [options] infile ...\n"
    "Assembles a 3D dataset from an ASCII list of coordinates and\n"
    "(optionally) values.\n"
    "\n"
    "Options:\n"
    "  -prefix ppp  = 'ppp' is the prefix for the output dataset\n"
    "                   [default = undump].\n"
    "  -master mmm  = 'mmm' is the master dataset, whose geometry\n"
    "    *OR*           will determine the geometry of the output.\n"
    "  -dimen I J K = Sets the dimensions of the output dataset to\n"
    "                   be I by J by K voxels.  (Each I, J, and K\n"
    "                   must be >= 1.)  This option can be used to\n"
    "                   create a dataset of a specific size for test\n"
    "                   purposes, when no suitable master exists.\n"
    "          ** N.B.: Exactly one of -master or -dimen must be given.\n"
    "  -mask kkk    = This option specifies a mask dataset 'kkk', which\n"
    "                   will control which voxels are allowed to get\n"
    "                   values set.  If the mask is present, only\n"
    "                   voxels that are nonzero in the mask can be\n"
    "                   set in the new dataset.\n"
    "                   * A mask can be created with program 3dAutomask.\n"
    "                   * Combining a mask with sphere insertion makes\n"
    "                     a lot of sense (to me, at least).\n"
    "  -datum type  = 'type' determines the voxel data type of the\n"
    "                   output, which may be byte, short, or float\n"
    "                   [default = short].\n"
    "  -dval vvv    = 'vvv' is the default value stored in each\n"
    "                   input voxel that does not have a value\n"
    "                   supplied in the input file [default = 1].\n"
    "  -fval fff    = 'fff' is the fill value, used for each voxel\n"
    "                   in the output dataset that is NOT listed\n"
    "                   in the input file [default = 0].\n"
    "  -ijk         = Coordinates in the input file are (i,j,k) index\n"
    "       *OR*        triples, as might be output by 3dmaskdump.\n"
    "  -xyz         = Coordinates in the input file are (x,y,z)\n"
    "                   spatial coordinates, in mm.  If neither\n"
    "                   -ijk or -xyz is given, the default is -ijk.\n"
    "          ** N.B.: -xyz can only be used with -master. If -dimen\n"
    "                   is used to specify the size of the output dataset,\n"
    "                   (x,y,z) coordinates are not defined (until you\n"
    "                   use 3drefit to define the spatial structure).\n"
    "  -srad rrr    = Specifies that a sphere of radius 'rrr' will be\n"
    "                   filled about each input (x,y,z) or (i,j,k) voxel.\n"
    "                   If the radius is not given, or is 0, then each\n"
    "                   input data line sets the value in only one voxel.\n"
    "                   * If '-master' is used, then 'rrr' is in mm.\n"
    "                   * If '-dimen' is used, then 'rrr' is in voxels.\n"
    "  -orient code = Specifies the coordinate order used by -xyz.\n"
    "                   The code must be 3 letters, one each from the pairs\n"
    "                   {R,L} {A,P} {I,S}.  The first letter gives the\n"
    "                   orientation of the x-axis, the second the orientation\n"
    "                   of the y-axis, the third the z-axis:\n"
    "                     R = right-to-left         L = left-to-right\n"
    "                     A = anterior-to-posterior P = posterior-to-anterior\n"
    "                     I = inferior-to-superior  S = superior-to-inferior\n"
    "                   If -orient isn't used, then the coordinate order of the\n"
    "                   -master dataset is used to interpret (x,y,z) inputs.\n"
    "          ** N.B.: If -dimen is used (which implies -ijk), then the\n"
    "                   only use of -orient is to specify the axes ordering\n"
    "                   of the output dataset.  If -master is used instead,\n"
    "                   the output dataset's axes ordering is the same as the\n"
    "                   -master dataset's, regardless of -orient.\n"
    "  -head_only   =  A 'secret' option for creating only the .HEAD file which\n"
    "                  gets exploited by the AFNI matlab library function\n"
    "                  New_HEAD.m\n"
    "\n"
    "Input File Format:\n"
    " The input file(s) are ASCII files, with one voxel specification per\n"
    " line.  A voxel specification is 3 numbers (-ijk or -xyz coordinates),\n"
    " with an optional 4th number giving the voxel value.  For example:\n"
    "\n"
    "   1 2 3 \n"
    "   3 2 1 5\n"
    "   5.3 6.2 3.7\n"
    "   // this line illustrates a comment\n"
    "\n"
    " The first line puts a voxel (with value given by -dval) at point\n"
    " (1,2,3).  The second line puts a voxel (with value 5) at point (3,2,1).\n"
    " The third line puts a voxel (with value given by -dval) at point\n"
    " (5.3,6.2,3.7).  If -ijk is in effect, and fractional coordinates\n"
    " are given, they will be rounded to the nearest integers; for example,\n"
    " the third line would be equivalent to (i,j,k) = (5,6,4).\n"
    "\n"
    "Notes:\n"
    "* This program creates a 1 sub-brick file.  You can 'glue' multiple\n"
    "   files together using 3dbucket or 3dTcat to make multi-brick datasets.\n"
    "\n"
    "* If one input filename is '-', then stdin will be used for input.\n"
    "\n"
    "* If no input files are given, an 'empty' dataset is created.\n"
    "   For example, to create an all zero dataset with 1 million voxels:\n"
    "     3dUndump -dimen 100 100 100 -prefix AllZero\n"
    "\n"
    "* By default, the output dataset is of type '-fim', unless the -master\n"
    "   dataset is an anat type. You can change the output type using 3drefit.\n"
    "\n"
    "* You could use program 1dcat to extract specific columns from a\n"
    "   multi-column rectangular file (e.g., to get a specific sub-brick\n"
    "   from the output of 3dmaskdump), and use the output of 1dcat as input\n"
    "   to this program.\n"
    "\n"
    "* [19 Feb 2004] The -mask and -srad options were added this day.\n"
    "   Also, a fifth value on an input line, if present, is taken as a\n"
    "   sphere radius to be used for that input point only.  Thus, input\n"
    "      3.3 4.4 5.5 6.6 7.7\n"
    "   means to put the value 6.6 into a sphere of radius 7.7 mm centered\n"
    "   about (x,y,z)=(3.3,4.4,5.5).\n"
    "\n"
    "* [10 Nov 2008] Commas (',') inside an input line are converted to\n"
    "   spaces (' ') before the line is interpreted.  This feature is for\n"
    "   convenience for people writing files in CSV (Comma Separated Values)\n"
    "   format.\n"
    "\n"
    "* [31 Dec 2008] Inputs of 'NaN' are explicitly converted to zero, and\n"
    "  a warning message is printed.  AFNI programs do not deal with NaN\n"
    "  floating point values!\n"
    "\n"
    "-- RWCox -- October 2000\n"
   ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
}

/*---------------------------------------------------------------------------*/

#define NBUF 1024  /* line buffer size */

int main( int argc , char * argv[] )
{
   int do_ijk=1 , dimen_ii=0 , dimen_jj=0 , dimen_kk=0 , datum=MRI_short ;
   THD_3dim_dataset *mset=NULL ;
   char *prefix="undump" , *orcode=NULL ;
   THD_coorder cord ;
   float dval_float=1.0 , fval_float=0.0 , *fbr=NULL ;
   short dval_short=1   , fval_short=0   , *sbr=NULL ;
   byte  dval_byte =1   , fval_byte =0   , *bbr=NULL , *mmask=NULL ;

   FILE *fp ;
   THD_3dim_dataset *dset , *maskset=NULL ;
   int iarg , ii,jj,kk,ll,ijk , nx,ny,nz , nxyz , nn , do_head_only=0;
   float      xx,yy,zz,vv=0.0 ;
   short               sv=0   ;
   byte                bv=0   ;
   char linbuf[NBUF] , *cp ;

   float xxdown,xxup , yydown,yyup , zzdown,zzup ;

   float srad=0.0 , vrad,rii,rjj,rkk,qii,qjj,qkk , dx,dy,dz ;  /* 19 Feb 2004 */

   /*-- help? --*/

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ) Syntax() ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dUndump main") ; machdep() ; PRINT_VERSION("3dUndump");

   machdep() ;
   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3dUndump",argc,argv) ;

   /*-- command line options --*/

   do_head_only = 0 ;
   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-") == 0 ){    /* a single - is an input filename */
         break ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         if( iarg+1 >= argc )
            ERROR_exit("-prefix: no argument follows!?") ;
         else if( !THD_filename_ok(argv[++iarg]) )
            ERROR_exit("-prefix: Illegal prefix given!") ;
         prefix = argv[iarg] ;
         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-master") == 0 ){
         if( iarg+1 >= argc )
            ERROR_exit("-master: no argument follows!?") ;
         else if( mset != NULL )
            ERROR_exit("-master: can't have two -master options!") ;
         else if( dimen_ii > 0 )
            ERROR_exit("-master: conflicts with previous -dimen!") ;

         mset = THD_open_dataset( argv[++iarg] ) ;
         if( mset == NULL )
            ERROR_exit("-master: can't open dataset" ) ;

         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-mask") == 0 ){
        if( iarg+1 >= argc )
          ERROR_exit("-mask: no argument follows!?") ;
        else if( maskset != NULL )
          ERROR_exit("-mask: can't have two -mask options!") ;

        maskset = THD_open_dataset( argv[++iarg] ) ;
        if( maskset == NULL )
          ERROR_exit("-mask: can't open dataset" ) ;

        iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-dimen") == 0 ){
         if( iarg+3 >= argc )
            ERROR_exit("-dimen: don't have 3 arguments following!?") ;
         else if( mset != NULL )
            ERROR_exit("-dimen: conflicts with previous -master!") ;
         else if( dimen_ii > 0 )
            ERROR_exit("-dimen: can't have two -dimen options!") ;
         dimen_ii = strtol(argv[++iarg],NULL,10) ;
         dimen_jj = strtol(argv[++iarg],NULL,10) ;
         dimen_kk = strtol(argv[++iarg],NULL,10) ;
         if( dimen_ii < 1 || dimen_jj < 1 || dimen_kk < 1 )
            ERROR_exit("-dimen: values following are not all >= 1!") ;

         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-datum") == 0 ){
         if( ++iarg >= argc )
            ERROR_exit("-datum: no argument follows?!") ;

         if( strcmp(argv[iarg],"short") == 0 )
            datum = MRI_short ;
         else if( strcmp(argv[iarg],"float") == 0 )
            datum = MRI_float ;
         else if( strcmp(argv[iarg],"byte") == 0 )
            datum = MRI_byte ;
         else
            ERROR_exit("-datum: illegal type given!") ;

         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-srad") == 0 ){   /* 19 Feb 2004 */
        if( iarg+1 >= argc )
          ERROR_exit("-srad: no argument follows!?") ;

        srad = strtod( argv[++iarg] , NULL ) ;
        if( srad <= 0.0 || thd_floatscan(1,&srad) ){
          WARNING_message("-srad value of %g is ignored!",srad);
          srad = 0.0 ;
        }
        iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-dval") == 0 ){
         if( iarg+1 >= argc )
            ERROR_exit("-dval: no argument follows!?") ;

         dval_float = strtod( argv[++iarg] , NULL ) ;
         if( thd_floatscan(1,&dval_float) )
           ERROR_exit("Illegal value entered for -dval!") ;
         dval_short = (short) rint(dval_float) ;
         dval_byte  = (byte)  dval_short ;
         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-fval") == 0 ){
         if( iarg+1 >= argc )
            ERROR_exit("-fval: no argument follows!?") ;

         fval_float = strtod( argv[++iarg] , NULL ) ;
         if( thd_floatscan(1,&fval_float) )
           ERROR_exit("Illegal value entered for -fval!") ;
         fval_short = (short) rint(fval_float) ;
         fval_byte  = (byte)  fval_short ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-ijk") == 0 ){
         do_ijk = 1 ;
         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-xyz") == 0 ){
         do_ijk = 0 ;
         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-head_only") == 0 ){
         do_head_only = 1 ;
         iarg++ ; continue ;
      }

      /*-----*/
      if( strcmp(argv[iarg],"-orient") == 0 ){
         int xx,yy,zz ;
         if( iarg+1 >= argc )
            ERROR_exit("-orient: no argument follows!?") ;

         orcode = argv[++iarg] ;
         if( strlen(orcode) != 3 )
            ERROR_exit("-orient: illegal argument follows") ;

         xx = ORCODE(orcode[0]) ; yy = ORCODE(orcode[1]) ; zz = ORCODE(orcode[2]) ;
         if( xx < 0 || yy < 0 || zz < 0 || !OR3OK(xx,yy,zz) )
            ERROR_exit("-orient: illegal argument follows") ;

         iarg++ ; continue ;
      }

      /*-----*/

      ERROR_exit("Unknown option: %s",argv[iarg]) ;

   } /* end of loop over command line options */

   /*-- check for inconsistencies --*/

#if 0
   if( iarg >= argc && !do_head_only)
      ERROR_exit("No input files on command line!?") ;
#endif

   if( do_ijk == 0 && mset == NULL )
      ERROR_exit("Can't use -xyz without -master also!") ;

   if( mset == NULL && dimen_ii < 2 )
      ERROR_exit("Must use exactly one of -master or -dimen options on command line");

   if( (datum == MRI_short && dval_short == fval_short) ||
       (datum == MRI_float && dval_float == fval_float) ||
       (datum == MRI_byte  && dval_byte  == fval_byte )   ){

      WARNING_message("-dval and -fval are the same!") ;
   }

   /*-- set orcode to value from -master, if this is needed --*/

   if( mset != NULL && do_ijk == 0 && orcode == NULL ){
      orcode = malloc(4) ;
      orcode[0] = ORIENT_typestr[mset->daxes->xxorient][0] ;
      orcode[1] = ORIENT_typestr[mset->daxes->yyorient][0] ;
      orcode[2] = ORIENT_typestr[mset->daxes->zzorient][0] ;
      orcode[3] = '\0' ;
   }

   THD_coorder_fill( orcode , &cord ) ;  /* setup coordinate order */

   /*-- make empty dataset --*/

   if( mset != NULL ){                 /* from -master */

      dset = EDIT_empty_copy( mset ) ;
      EDIT_dset_items( dset ,
                          ADN_prefix    , prefix ,
                          ADN_datum_all , datum ,
                          ADN_nvals     , 1 ,
                          ADN_ntt       , 0 ,
                          ADN_func_type , ISANAT(mset) ? mset->func_type
                                                       : FUNC_FIM_TYPE ,

                          ADN_directory_name , "./" ,
                       ADN_none ) ;

   } else {                            /* from nothing */

     THD_ivec3 iv_nxyz   , iv_xyzorient ;
     THD_fvec3 fv_xyzorg , fv_xyzdel ;

     LOAD_IVEC3( iv_nxyz , dimen_ii , dimen_jj , dimen_kk ) ;
     LOAD_IVEC3( iv_xyzorient , cord.xxor , cord.yyor , cord.zzor ) ;
     LOAD_FVEC3( fv_xyzdel ,
                 ORIENT_sign[iv_xyzorient.ijk[0]]=='+' ? 1.0 : -1.0 ,
                 ORIENT_sign[iv_xyzorient.ijk[1]]=='+' ? 1.0 : -1.0 ,
                 ORIENT_sign[iv_xyzorient.ijk[2]]=='+' ? 1.0 : -1.0  ) ;
     LOAD_FVEC3( fv_xyzorg ,
                 ORIENT_sign[iv_xyzorient.ijk[0]]=='+' ? -0.5*dimen_ii : 0.5*dimen_ii,
                 ORIENT_sign[iv_xyzorient.ijk[1]]=='+' ? -0.5*dimen_jj : 0.5*dimen_jj,
                 ORIENT_sign[iv_xyzorient.ijk[2]]=='+' ? -0.5*dimen_kk : 0.5*dimen_kk ) ;

     dset = EDIT_empty_copy( NULL ) ;

     EDIT_dset_items( dset ,
                       ADN_nxyz      , iv_nxyz ,
                       ADN_xyzdel    , fv_xyzdel ,
                       ADN_xyzorg    , fv_xyzorg ,
                       ADN_xyzorient , iv_xyzorient ,
                       ADN_prefix    , prefix ,
                       ADN_datum_all , datum ,
                       ADN_nvals     , 1 ,
                       ADN_ntt       , 0 ,
                       ADN_type      , HEAD_FUNC_TYPE ,
                       ADN_func_type , FUNC_FIM_TYPE ,
                    ADN_none ) ;
   }

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(dset)) )
      ERROR_exit("Output dataset already exists -- can't overwrite") ;

   if (do_head_only) {
      DSET_write_header(dset);
      exit(0);
   }

   /*-- make empty brick array for dataset --*/

   EDIT_substitute_brick( dset , 0 , datum , NULL ) ;  /* will make array */

   nx = DSET_NX(dset); ny = DSET_NY(dset); nz = DSET_NZ(dset); nxyz = nx*ny*nz;

   /* 19 Feb 2004: check and make mask if desired */

   if( maskset != NULL &&
       ( DSET_NX(maskset) != nx ||
         DSET_NY(maskset) != ny ||
         DSET_NZ(maskset) != nz   ) )
     ERROR_exit("-mask dataset doesn't match dimension of output dataset") ;

   if( maskset != NULL ){
     mmask = THD_makemask( maskset , 0 , 1.0,-1.0 ) ;
     if( mmask == NULL ){
       WARNING_message("Can't create mask for some reason!") ;
     } else {
       int nmask = THD_countmask( nxyz , mmask ) ;
       if( nmask == 0 ){
         WARNING_message("0 voxels in mask -- ignoring it!") ;
         free((void *)mmask) ; mmask = NULL ;
       } else {
         INFO_message("%d voxels found in mask",nmask) ;
       }
     }
     DSET_delete(maskset) ;
   }

   /*-- fill new dataset brick with the -fval value --*/

   switch( datum ){
      case MRI_short:
         sbr = (short *) DSET_BRICK_ARRAY(dset,0) ;
         for( ii=0 ; ii < nxyz ; ii++ ) sbr[ii] = fval_short ;
      break ;

      case MRI_float:
         fbr = (float *) DSET_BRICK_ARRAY(dset,0) ;
         for( ii=0 ; ii < nxyz ; ii++ ) fbr[ii] = fval_float ;
      break ;

      case MRI_byte:
         bbr = (byte *) DSET_BRICK_ARRAY(dset,0) ;
         for( ii=0 ; ii < nxyz ; ii++ ) bbr[ii] = fval_byte ;
      break ;
   }

   /* 24 Nov 2000: get the bounding box for the dataset */

   dx = fabs(dset->daxes->xxdel) ; if( dx <= 0.0 ) dx = 1.0 ;
   dy = fabs(dset->daxes->yydel) ; if( dy <= 0.0 ) dy = 1.0 ;
   dz = fabs(dset->daxes->zzdel) ; if( dz <= 0.0 ) dz = 1.0 ;

#ifndef EXTEND_BBOX
   xxdown = dset->daxes->xxmin - 0.501 * dx ;
   xxup   = dset->daxes->xxmax + 0.501 * dx ;
   yydown = dset->daxes->yymin - 0.501 * dy ;
   yyup   = dset->daxes->yymax + 0.501 * dy ;
   zzdown = dset->daxes->zzmin - 0.501 * dz ;
   zzup   = dset->daxes->zzmax + 0.501 * dz ;
#else
   xxdown = dset->daxes->xxmin ;
   xxup   = dset->daxes->xxmax ;
   yydown = dset->daxes->yymin ;
   yyup   = dset->daxes->yymax ;
   zzdown = dset->daxes->zzmin ;
   zzup   = dset->daxes->zzmax ;
#endif

   /*-- loop over input files and read them line by line --*/

   if( iarg == argc )
     WARNING_message("No input files ==> creating empty dataset") ;

   for( ; iarg < argc ; iarg++ ){  /* iarg is already set at start of this loop */

      /* get input file ready to read */

      if( strcmp(argv[iarg],"-") == 0 ){  /* stdin */
         fp = stdin ;
      } else {                            /* OK, open the damn file */
         fp = fopen( argv[iarg] , "r" ) ;
         if( fp == NULL ){
            WARNING_message("Can't open input file %s -- skipping it" ,
                            argv[iarg]) ;
            continue ;                    /* skip to end of iarg loop */
         }
      }

      /* read lines, process and store */

      ll = 0 ;
      while(1){
         ll++ ;                               /* line count */
         cp = fgets( linbuf , NBUF , fp ) ;
         if( cp == NULL ) break ;             /* end of file => end of loop */
         kk = strlen(linbuf) ;
         if( kk == 0 ) continue ;             /* empty line => get next line */

         /* find 1st nonblank */

         for( ii=0 ; ii < kk && isspace(linbuf[ii]) ; ii++ ) ; /* nada */
         if( ii == kk ) continue ;                                 /* all blanks */
         if( linbuf[ii] == '/' && linbuf[ii+1] == '/' ) continue ; /* comment */
         if( linbuf[ii] == '#'                        ) continue ; /* comment */

         /* changes commas to blanks [10 Nov 2008] */

         for( jj=ii ; jj < kk ; jj++ ) if( linbuf[ii] == ',' ) linbuf[ii] = ' ' ;

         /* scan line for data */

         vv   = dval_float ;   /* if not scanned in below, use the default value */
         vrad = srad ;         /* 19 Feb 2004: default sphere radius */
         nn   = sscanf(linbuf+ii , "%f%f%f%f%f" , &xx,&yy,&zz,&vv,&vrad ) ;
         if( nn < 3 ){
           WARNING_message("File %s line %d: incomplete",argv[iarg],ll) ;
           continue ;
         }
         if( thd_floatscan(1,&vv) ){
           WARNING_message("File %s line %d: replaced illegal value with 0",
                           argv[iarg],ll ) ;
         }
         if( thd_floatscan(1,&vrad) ){
           WARNING_message("File %s line %d: replaced illegal radius with 0",
                           argv[iarg],ll ) ;
         }

         /* get voxel index into (ii,jj,kk) */

         if( do_ijk ){   /* inputs are (ii,jj,kk) themselves */

            ii = (int) rint(xx) ; jj = (int) rint(yy) ; kk = (int) rint(zz) ;
            if( ii < 0 || ii >= nx ){
               WARNING_message("File %s line %d: i index=%d is invalid",
                               argv[iarg],ll,ii) ;
               continue ;
            }
            if( jj < 0 || jj >= ny ){
               WARNING_message("File %s line %d: j index=%d is invalid",
                               argv[iarg],ll,jj) ;
               continue ;
            }
            if( kk < 0 || kk >= nz ){
               WARNING_message("File %s line %d: k index=%d is invalid",
                               argv[iarg],ll,kk) ;
               continue ;
            }

         } else {  /* inputs are coordinates => must convert to index */

            THD_fvec3 mv , dv ;                              /* temp vectors */
            THD_ivec3 iv ;

            THD_coorder_to_dicom( &cord , &xx,&yy,&zz ) ;    /* to Dicom order */
            LOAD_FVEC3( dv , xx,yy,zz ) ;
            mv = THD_dicomm_to_3dmm( dset , dv ) ;           /* to Dataset order */

            /* 24 Nov 2000: check (xx,yy,zz) for being inside the box */

            if( mv.xyz[0] < xxdown || mv.xyz[0] > xxup ){
               WARNING_message("File %s line %d: x coord=%g is outside %g .. %g" ,
                               argv[iarg],ll,mv.xyz[0] , xxdown,xxup ) ;
               continue ;
            }
            if( mv.xyz[1] < yydown || mv.xyz[1] > yyup ){
               WARNING_message("File %s line %d: y coord=%g is outside %g .. %g" ,
                               argv[iarg],ll,mv.xyz[1] , yydown , yyup ) ;
               continue ;
            }
            if( mv.xyz[2] < zzdown || mv.xyz[2] > zzup ){
               WARNING_message("File %s line %d: z coord=%g is outside %g .. %g" ,
                               argv[iarg],ll,mv.xyz[2] , zzdown , zzup ) ;
               continue ;
            }

            iv = THD_3dmm_to_3dind( dset , mv ) ;            /* to Dataset index */
            ii = iv.ijk[0]; jj = iv.ijk[1]; kk = iv.ijk[2];  /* save */
         }

         /* now load individual voxel (ii,jj,kk) */

         ijk = ii + jj*nx + kk*nx*ny ;
         if( mmask == NULL || mmask[ijk] ){
          switch( datum ){
            case MRI_float:{
              if( fbr[ijk] != fval_float && fbr[ijk] != vv )
                WARNING_message("Overwrite voxel %d %d %d",ii,jj,kk) ;
              fbr[ijk] = vv ;
            }
            break ;
            case MRI_short:{
              static int first=1 ;
              sv = SHORTIZE(vv) ;
              if( first && vv != (float)sv ){
                WARNING_message("Truncating non-short data values to 16-bit integers!") ;
                first = 0 ;
              }
              if( sbr[ijk] != fval_short && sbr[ijk] != sv )
                WARNING_message("Overwrite voxel %d %d %d",ii,jj,kk) ;
              sbr[ijk] = sv ;
            }
            break ;
            case MRI_byte:{
              static int first=1 ;
              bv = BYTEIZE(vv) ;
              if( first && vv != (float)bv ){
                WARNING_message("Truncating non-byte data values to 8-bit integers!") ;
                first = 0 ;
              }
              if( bbr[ijk] != fval_byte && bbr[ijk] != bv )
                WARNING_message("Overwrite voxel %d %d %d",ii,jj,kk) ;
              bbr[ijk] = bv ;
            }
            break ;
          }
         }

         /* 19 Feb 2004:
             Make up radius of ellipsoid in voxel indexes
             Will put value vv into all voxels (aa,bb,cc) with
               (aa-ii)**2/qii + (bb-jj)**2/qjj + (cc-kk)**2/qkk <= 1.0 */

         vrad *= 1.00001 ;                               /* expand a little */
         rii = vrad/dx ; rjj = vrad/dy ; rkk = vrad/dz ;
         qii = rii*rii ; qjj = rjj*rjj ; qkk = rkk*rkk ;

         if( rii >= 1.0 || rjj >= 1.0 || rkk >= 1.0 ){
           int aa,bb,cc , abot,atop,bbot,btop,cbot,ctop; float rr;
           abot = ii-(int)rint(rii) ; atop = ii+(int)rint(rii) ;
           if( abot < 0 ) abot = 0 ; if( atop >= nx ) atop = nx-1 ;
           bbot = jj-(int)rint(rjj) ; btop = jj+(int)rint(rjj) ;
           if( bbot < 0 ) bbot = 0 ; if( btop >= ny ) btop = ny-1 ;
           cbot = kk-(int)rint(rkk) ; ctop = kk+(int)rint(rkk) ;
           if( cbot < 0 ) cbot = 0 ; if( ctop >= nz ) ctop = nz-1 ;
           for( cc=cbot ; cc <= ctop ; cc++ ){
             for( bb=bbot ; bb <= btop ; bb++ ){
               for( aa=abot ; aa <= atop ; aa++ ){
                 rr =  (aa-ii)*(aa-ii)/qii
                     + (bb-jj)*(bb-jj)/qjj + (cc-kk)*(cc-kk)/qkk ;
                 if( rr <= 1.00001 ){
                   ijk = aa + bb*nx + cc*nx*ny ;    /* (aa,bb,cc) in dataset */
                   if( mmask == NULL || mmask[ijk] ){
                     switch( datum ){
                       case MRI_float: fbr[ijk] = vv ; break ;
                       case MRI_short: sbr[ijk] = sv ; break ;
                       case MRI_byte:  bbr[ijk] = bv ; break ;
                     }
                   }
                 }
               }
             }
           }
         }  /* 19 Feb 2004: end of inserting a sphere */

      } /* end of loop over input lines */

      /* close input file */

      if( fp != stdin ) fclose( fp ) ;

   } /* end of loop over input files */

   dset_floatscan(dset) ;
   tross_Make_History( "3dUndump" , argc,argv , dset ) ;
   DSET_write(dset) ;
   WROTE_DSET(dset) ;
   exit(0) ;
}
