#include "mrilib.h"

/*---------------------------------------------------------------------------*/

void A2A_help(void)
{
  printf("Usage: 3dANALYZEtoAFNI [options] file1.hdr file2.hdr ...\n"
         "This program constructs a 'volumes' stored AFNI dataset\n"
         "from the ANALYZE-75 files file1.img file2.img ....\n"
         "In this type of dataset, there is only a .HEAD file; the\n"
         ".BRIK file is replaced by the collection of .img files.\n"
         "- Other AFNI programs can read (but not write) this type\n"
         "  of dataset.\n"
         "- The advantage of using this type of dataset vs. one created\n"
         "   with to3d is that you don't have to duplicate the image data\n"
         "   into a .BRIK file, thus saving disk space.\n"
         "- The disadvantage of using 'volumes' for a multi-brick dataset\n"
         "   is that all the .img files must be kept with the .HEAD file\n"
         "   if you move the dataset around.\n"
         "- The .img files must be in the same directory as the .HEAD file.\n"
         "- Note that you put the .hdr files on the command line, but it is\n"
         "   the .img files that will be named in the .HEAD file.\n"
         "- After this program is run, you must keep the .img files with\n"
         "   the output .HEAD file.  AFNI doesn't need the .hdr files, but\n"
         "   other programs (e.g., FSL, SPM) will want them as well.\n"
         "\n"
         "Options:\n"
         " -prefix ppp   = Save the dataset with the prefix name 'ppp'.\n"
         "                  [default='a2a']\n"
         " -view vvv     = Save the dataset in the 'vvv' view, where\n"
         "                  'vvv' is one of 'orig', 'acpc', or 'tlrc'.\n"
         "                  [default='orig']\n"
         "\n"
         " -TR ttt       = For multi-volume datasets, create it as a\n"
         "                  3D+time dataset with TR set to 'ttt'.\n"
         " -fbuc         = For multi-volume datasets, create it as a\n"
         "                  functional bucket dataset.\n"
         " -abuc         = For multi-volume datasets, create it as an\n"
         "                  anatomical bucket dataset.\n"
         "   ** If more than one ANALYZE file is input, and none of the\n"
         "       above options is given, the default is as if '-TR 1s'\n"
         "       was used.\n"
         "   ** For single volume datasets (1 ANALYZE file input), the\n"
         "       default is '-abuc'.\n"
         "\n"
         " -geomparent g = Use the .HEAD file from dataset 'g' to set\n"
         "                  the geometry of this dataset.\n"
         "   ** If you don't use -geomparent, then the following options\n"
         "       can be used to specify the geometry of this dataset:\n"
         " -orient code  = Tells the orientation of the 3D volumes.  The code\n"
         "                  must be 3 letters, one each from the pairs {R,L}\n"
         "                  {A,P} {I,S}.  The first letter gives the orientation\n"
         "                  of the x-axis, the second the orientation of the\n"
         "                  y-axis, the third the z-axis:\n"
         "                   R = right-to-left         L = left-to-right\n"
         "                   A = anterior-to-posterior P = posterior-to-anterior\n"
         "                   I = inferior-to-superior  S = superior-to-inferior\n"
         " -zorigin dz   = Puts the center of the 1st slice off at the\n"
         "                  given distance ('dz' in mm).  This distance\n"
         "                  is in the direction given by the corresponding\n"
         "                  letter in the -orient code.  For example,\n"
         "                    -orient RAI -zorigin 30\n"
         "                  would set the center of the first slice at\n"
         "                  30 mm Inferior.\n"
         "   ** If the above options are NOT used to specify the geometry\n"
         "       of the dataset, then the default is '-orient RAI', and the\n"
         "       z origin is set to center the slices about z=0.\n"
         "\n"
         " It is likely that you will want to patch up the .HEAD file using\n"
         " program 3drefit.\n"
         "\n"
         " -- RWCox - June 2002.\n"
      ) ;
}

/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 ;
   THD_3dim_dataset *dset ;    /* output dataset */
   int nvals , ii , jj , kk ;

   MRI_IMARR *anar ;      /* stuff from ANALYZE headers */
   MRI_IMAGE *anim ;
   int nxan,nyan,nzan , an_datum , an_swapped ;
   float dxan=-1.0,dyan,dzan ;
   char anor[8] = "\0" ;

   THD_ivec3 nxyz , orixyz ;
   THD_fvec3 dxyz , orgxyz ;

   float TR=1.0 ; int tunits=UNITS_SEC_TYPE ;
   int is_fbuc=0 , is_abuc=0 , is_3dtime=0 ;
   int view_type=VIEW_ORIGINAL_TYPE ;
   char *prefix="a2a" ;
   int xorient=-1, yorient=-1, zorient=-1 ;
   int use_zoff=0,use_xoff=0,use_yoff=0 ; float zoff,xoff,yoff ;
   THD_3dim_dataset *gset=NULL ;  /* geometry parent */
   float *fac ;

   char **flab , *fatr ;

   /*-- help the poor user? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){ A2A_help(); exit(0); }

   /*-- parse options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     /* -prefix */

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ){
         fprintf(stderr,"** -prefix needs an argument!\n"); exit(1);
       }
       prefix = argv[iarg] ;
       if( !THD_filename_ok(prefix) ){
         fprintf(stderr,"** Illegal prefix!\n"); exit(1);
       }
       if( strstr(prefix,"/") != NULL ){
         fprintf(stderr,"** Can't use directory names in the prefix!\n"); exit(1);
       }
       iarg++ ; continue ;
     }

     /* -view */

     if( strcmp(argv[iarg],"-view") == 0 ){
       char *str ;
       if( ++iarg >= argc ){
         fprintf(stderr,"** -view needs an argument!\n"); exit(1);
       }
       str = argv[iarg] ; if( str[0] == '+' ) str++ ;
       for( ii=FIRST_VIEW_TYPE ; ii <= LAST_VIEW_TYPE ; ii++ )
         if( strcmp(str,VIEW_codestr[ii]) == 0 ) break ;

       if( ii <= LAST_VIEW_TYPE ){
         view_type = ii ;
       } else {
         fprintf(stderr,"** Illegal -view code!\n"); exit(1);
       }
       iarg++ ; continue ;
     }

     /* -zorigin */

     if( strcmp(argv[iarg],"-zorigin") == 0 ){
       if( ++iarg >= argc ){
         fprintf(stderr,"** -zorigin needs an argument!\n"); exit(1);
       }
       zoff = strtod( argv[iarg] , NULL ) ;
       use_zoff = 1 ;
       iarg++ ; continue ;
     }

     /* -orient */

#define ORCODE(aa) \
  ( (aa)=='R' ? ORI_R2L_TYPE : (aa)=='L' ? ORI_L2R_TYPE : \
    (aa)=='P' ? ORI_P2A_TYPE : (aa)=='A' ? ORI_A2P_TYPE : \
    (aa)=='I' ? ORI_I2S_TYPE : (aa)=='S' ? ORI_S2I_TYPE : ILLEGAL_TYPE )

#define OR3OK(x,y,z) ( ((x)&6) + ((y)&6) + ((z)&6) == 6 )

     if( strcmp(argv[iarg],"-orient") == 0 ){
       char acod ;

       if( ++iarg >= argc ){
         fprintf(stderr,"** -orient needs an argument!\n"); exit(1);
       }
       if( strlen(argv[iarg]) != 3 ){
         fprintf(stderr,"** Illegal -orient code!\n"); exit(1);
       }
       acod = toupper(argv[iarg][0]) ; xorient = ORCODE(acod) ;
       acod = toupper(argv[iarg][1]) ; yorient = ORCODE(acod) ;
       acod = toupper(argv[iarg][2]) ; zorient = ORCODE(acod) ;
       if( xorient<0 || yorient<0 || zorient<0 ||
           ! OR3OK(xorient,yorient,zorient)      ){
         fprintf(stderr,"** Unusable -orient code!\n"); exit(1);
       }
       iarg++ ; continue ;
     }

      /* -geomparent */

     if( strcmp(argv[iarg],"-geomparent") == 0 ){
       if( ++iarg >= argc ){
         fprintf(stderr,"** -geomparent needs an argument!\n"); exit(1);
       }
       gset = THD_open_dataset( argv[iarg] ) ;
       if( !ISVALID_DSET(gset) ){
         fprintf(stderr,"** Can't open -geomparent dataset!\n"); exit(1);
       }
       iarg++ ; continue ;
     }

     /* -TR */

     if( strcmp(argv[iarg],"-TR") == 0 ){
       char *eptr ;
       if( ++iarg >= argc ){
         fprintf(stderr,"** -geomparent needs an argument!\n"); exit(1);
       }
       TR = strtod( argv[iarg] , &eptr ) ;
       if( TR <= 0.0 ){
         fprintf(stderr,"** -TR needs a positive value after it!\n"); exit(1);
       }

       if( strcmp(eptr,"ms")==0 || strcmp(eptr,"msec")==0 ){
          tunits = UNITS_MSEC_TYPE ;
       } else if( strcmp(eptr,"s")==0 || strcmp(eptr,"sec")==0 ){
          tunits = UNITS_SEC_TYPE ;
       } else if( strcmp(eptr,"Hz")==0 || strcmp(eptr,"Hertz")==0 ){
          tunits = UNITS_HZ_TYPE ;
       }

       is_3dtime = 1 ; is_abuc = is_fbuc = 0 ;
       iarg++ ; continue ;
     }

     /* -fbuc */

     if( strcmp(argv[iarg],"-fbuc") == 0 ){
       is_fbuc = 1 ; is_abuc = is_3dtime = 0 ;
       iarg++ ; continue ;
     }

     /* -abuc */

     if( strcmp(argv[iarg],"-abuc") == 0 ){
       is_abuc = 1 ; is_fbuc = is_3dtime = 0 ;
       iarg++ ; continue ;
     }

     /** don't know this one **/

     fprintf(stderr,"** Illegal option %s\n",argv[iarg]); exit(1);
   }

   /*-- check number of remaining args --*/

   nvals = argc - iarg ;
   if( nvals <= 0 ){
     fprintf(stderr,"** No ANALYZE files on command line!?\n"); exit(1);
   }

   /*-- try to read each ANALYZE file and glean info from it --*/

   CLEAR_MRILIB_globals ;  /* setup */

   fac = (float *) malloc(sizeof(float)*nvals) ;

   for( ii=iarg ; ii < argc ; ii++ ){

     if( strstr(argv[ii],"/") != NULL ){
       fprintf(stderr,"** ANALYZE files must all be in current directory!\n") ;
       exit(1) ;
     }

     anar = mri_read_analyze75( argv[ii] ) ;  /* read it */

     if( anar == NULL || IMARR_COUNT(anar) == 0 ){
       fprintf(stderr,"** Can't read %s as ANALYZE-75 format .hdr file!\n",argv[ii]);
       exit(1) ;
     }

     anim = IMARR_SUBIM(anar,0) ;     /* first 2D image */

     fac[ii-iarg] = anim->dv ;        /* save scale factor, if any */

     if( ii == iarg ){                /* first time in: store header values */

       nxan = anim->nx ; nyan = anim->ny ; nzan = IMARR_COUNT(anar) ;
       if( MRILIB_orients[0] != '\0' ) strcpy(anor,MRILIB_orients) ;
       an_datum = anim->kind ;
       if( anim->dw > 0.0 ){ dxan = anim->dx; dyan = anim->dy; dzan = anim->dz; }
       an_swapped = anim->was_swapped ;

     } else {                         /* check later sub-bricks */

       if( nxan != anim->nx || nyan != anim->ny || nzan != IMARR_COUNT(anar) ){
         fprintf(stderr,"** File %s has different dimensions than %s\n",
                 argv[ii] , argv[iarg] ) ;
         exit(1) ;
       }
       if( an_datum != anim->kind ){
         fprintf(stderr,"** File %s has different kind of data than %s\n",
                 argv[ii] , argv[iarg] ) ;
         exit(1) ;
       }
       if( an_swapped != anim->was_swapped ){
         fprintf(stderr,"** File %s %s byte-swapped, but %s %s\n",
                 argv[ii]   , (anim->was_swapped) ? "is" : "isn't" ,
                 argv[iarg] , (an_swapped)        ? "is" : "isn't"  ) ;
         exit(1) ;
       }
       if( anim->dw > 0.0 ){
         if( dxan < 0.0 ){
           dxan = anim->dx; dyan = anim->dy; dzan = anim->dz;
         } else {
           if( dxan != anim->dx || dyan != anim->dy || dzan != anim->dz ){
             fprintf(stderr,"++ WARNING: File %s has variant voxel sizes!\n",
                     argv[ii]) ;
           }
         }
       }
       if( anor[0] == '\0' && MRILIB_orients[0] != '\0' )
         strcpy(anor,MRILIB_orients) ;

     } /* end of checking later volumes for compatibility */

     /* destroy this data (we've sucked it dry) */

     DESTROY_IMARR(anar) ;

  } /* end of loop over ANALYZE files */

  /*-- check geomparent, if present --*/

  if( gset != NULL ){

    if( DSET_NX(gset)!=nxan || DSET_NY(gset)!=nyan || DSET_NZ(gset)!=nzan ){
      fprintf(stderr,"** geomparent and ANALYZE files have different dimensions!\n");
      exit(1) ;
    }

    if( xorient >= 0 ){
      fprintf(stderr,"++ WARNING: geomparent overrides -orient!\n") ;
    }
    xorient = gset->daxes->xxorient ;
    yorient = gset->daxes->yyorient ;
    zorient = gset->daxes->zzorient ;

    if( use_zoff ){
      fprintf(stderr,"++ WARNING: geomparent overrides -zorigin!\n") ;
    }
    use_xoff = use_yoff = use_zoff = 1 ;
    xoff = gset->daxes->xxorg ;
    yoff = gset->daxes->yyorg ;
    zoff = gset->daxes->zzorg ;

    dxan = gset->daxes->xxdel ;
    dyan = gset->daxes->yydel ;
    dzan = gset->daxes->zzdel ;

    if( gset->taxis != NULL ){
       TR     = gset->taxis->ttdel ;
       tunits = gset->taxis->units_type ;
    }

    DSET_delete(gset) ;  /* delete to save memory */

  } else {         /* don't have geomparent, so check if other data is OK */

    if( xorient < 0 ){
      xorient = ORI_R2L_TYPE ;
      yorient = ORI_A2P_TYPE ;
      zorient = ORI_I2S_TYPE ;
      fprintf(stderr,"++ WARNING: orientation defaults to RAI\n") ;
    }

    if( dxan <= 0.0 ){
      dxan = dyan = dzan = 1.0 ;
      fprintf(stderr,"++ WARNING: voxel size defaults to 1 mm\n") ;
    }

    if( ORIENT_sign[xorient] == '-' ) dxan = -dxan ;
    if( ORIENT_sign[yorient] == '-' ) dyan = -dyan ;
    if( ORIENT_sign[zorient] == '-' ) dzan = -dzan ;

  } /* end of setup for new dataset parameters */

  /*-- At last: create empty output dataset --*/

  dset = EDIT_empty_copy(NULL) ;

  nxyz.ijk[0] = nxan ; dxyz.xyz[0] = dxan ;
  nxyz.ijk[1] = nyan ; dxyz.xyz[1] = dyan ;
  nxyz.ijk[2] = nzan ; dxyz.xyz[2] = dzan ;

  orixyz.ijk[0] = xorient ;
  orixyz.ijk[1] = yorient ;
  orixyz.ijk[2] = zorient ;

  orgxyz.xyz[0] = (use_xoff) ? xoff : -0.5*(nxan-1)*dxan ;
  orgxyz.xyz[1] = (use_yoff) ? yoff : -0.5*(nyan-1)*dyan ;
  orgxyz.xyz[2] = (use_zoff) ? zoff : -0.5*(nzan-1)*dzan ;

  EDIT_dset_items( dset ,
                     ADN_prefix      , prefix ,
                     ADN_datum_all   , an_datum ,
                     ADN_nxyz        , nxyz ,
                     ADN_xyzdel      , dxyz ,
                     ADN_xyzorg      , orgxyz ,
                     ADN_xyzorient   , orixyz ,
                     ADN_nvals       , nvals ,
                     ADN_view_type   , view_type ,
                     ADN_brick_fac   , fac ,
                   ADN_none ) ;

  /*-- select dataset type (3D+time or bucket) --*/

  if( nvals == 1 && is_3dtime ){
    is_3dtime = 0; is_abuc = 1;
    fprintf(stderr,"++ WARNING: can't make a 3D+time dataset from 1 volume!\n") ;
  }

  if( !is_3dtime && !is_abuc && !is_fbuc ){
    if( nvals > 1 ) is_3dtime = 1 ;
    else            is_abuc   = 1 ;
  }

  if( is_3dtime ){
    EDIT_dset_items( dset ,
                       ADN_ntt      , nvals ,
                       ADN_ttorg    , 0.0 ,
                       ADN_ttdel    , TR ,
                       ADN_ttdur    , 0.0 ,
                       ADN_tunits   , tunits ,
                       ADN_type     , HEAD_ANAT_TYPE ,
                       ADN_func_type, ANAT_EPI_TYPE ,
                     ADN_none ) ;
  } else if( is_abuc ){
    EDIT_dset_items( dset ,
                       ADN_type     , HEAD_ANAT_TYPE ,
                       ADN_func_type, ANAT_BUCK_TYPE ,
                     ADN_none ) ;
  } else if( is_fbuc ){
    EDIT_dset_items( dset ,
                       ADN_type     , HEAD_FUNC_TYPE ,
                       ADN_func_type, FUNC_BUCK_TYPE ,
                     ADN_none ) ;
  }

  /*-- make filename array and store it in dataset header --*/

  flab = (char **) malloc(sizeof(char *)*nvals) ;

  kk = 0 ;
  for( ii=0 ; ii < nvals ; ii++ ){
    jj = strlen( argv[iarg+ii] ) ;             /* convert .hdr */
    flab[ii] = strdup( argv[iarg+ii] ) ;       /* filename to */
    strcpy( flab[ii]+jj-3 , "img" ) ;          /* .img name  */
    kk += (jj+2) ;
    THD_store_datablock_label( dset->dblk , ii , flab[ii] ) ;
  }

  fatr = malloc(kk) ; fatr[0] = '\0' ;         /* all filenames */
  for( ii=0 ; ii < nvals ; ii++ ){             /* into one big */
    strcat(fatr,flab[ii] ); strcat(fatr," ");  /* string      */
    free(flab[ii]) ;
  }

  /*-- setting this attribute marks the dataset as being
       stored by volumes, rather than all data in one .BRIK file --*/

  THD_set_string_atr( dset->dblk , "VOLUME_FILENAMES" , fatr ) ;
  free(fatr) ; free(flab) ;

  /*-- set byte ordering flag --*/

  jj = mri_short_order() ;  /* order of this CPU */

  if( an_swapped )
    dset->dblk->diskptr->byte_order = REVERSE_ORDER(jj) ;
  else
    dset->dblk->diskptr->byte_order = jj ;

  /*-- set history attribute --*/

  tross_Make_History( "3dANALYZEtoAFNI" , argc,argv , dset ) ;

  /*-- write dataset header --*/

  THD_write_3dim_dataset( NULL,NULL , dset , False ) ;

  fprintf(stderr,"++ Wrote dataset header %s\n",DSET_HEADNAME(dset)) ;
  exit(0) ;
}
