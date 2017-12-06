#include "mrilib.h"

#ifndef ORCODE
# define ORCODE(aa)                         \
   ( ((aa)=='R'||(aa)=='r') ? ORI_R2L_TYPE  \
    :((aa)=='L'||(aa)=='l') ? ORI_L2R_TYPE  \
    :((aa)=='P'||(aa)=='p') ? ORI_P2A_TYPE  \
    :((aa)=='A'||(aa)=='a') ? ORI_A2P_TYPE  \
    :((aa)=='I'||(aa)=='i') ? ORI_I2S_TYPE  \
    :((aa)=='S'||(aa)=='s') ? ORI_S2I_TYPE : ILLEGAL_TYPE )

# define OR3OK(x,y,z) ( ((x)&6) + ((y)&6) + ((z)&6) == 6 )
#endif

int main( int argc , char *argv[] )
{
   char *aname ;
   THD_3dim_dataset *dset ;
   int ii , scl ;
   MRI_IMAGE *im , *qim ;
   char *fname ;
   float fac ;

   int do_4D=0 , iarg=1 ;    /* 30 Sep 2002 */
   FILE *ifp=NULL ;

   int xxor=-1,yyor=0,zzor=0 , xdir=0,ydir=0,zdir=0;  /* 19 Mar 2003 */
   float                   xdel=0.0  ,ydel=0.0,zdel=0.0;
   char orient_code[4] ;

   /*-- help me if you can --*/

WARNING_message("This program (3dAFNItoANALYZE) is old, not maintained, and probably useless!") ;

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAFNItoANALYZE [-4D] [-orient code] aname dset\n"
             "Writes AFNI dataset 'dset' to 1 or more ANALYZE 7.5 format\n"
             ".hdr/.img file pairs (one pair for each sub-brick in the\n"
             "AFNI dataset).  The ANALYZE files will be named\n"
             "  aname_0000.hdr aname_0000.img   for sub-brick #0\n"
             "  aname_0001.hdr aname_0001.img   for sub-brick #1\n"
             "and so forth.  Each file pair will contain a single 3D array.\n"
             "\n"
             "* If the AFNI dataset does not include sub-brick scale\n"
             "  factors, then the ANALYZE files will be written in the\n"
             "  datum type of the AFNI dataset.\n"
             "* If the AFNI dataset does have sub-brick scale factors,\n"
             "  then each sub-brick will be scaled to floating format\n"
             "  and the ANALYZE files will be written as floats.\n"
             "* The .hdr and .img files are written in the native byte\n"
             "  order of the computer on which this program is executed.\n"
             "\n"
             "Options\n"
             "-------\n"
             "-4D [30 Sep 2002]:\n"
             " If you use this option, then all the data will be written to\n"
             " one big ANALYZE file pair named aname.hdr/aname.img, rather\n"
             " than a series of 3D files.  Even if you only have 1 sub-brick,\n"
             " you may prefer this option, since the filenames won't have\n"
             " the '_0000' appended to 'aname'.\n"
             "\n"
             "-orient code [19 Mar 2003]:\n"
             " This option lets you flip the dataset to a different orientation\n"
             " when it is written to the ANALYZE files.  The orientation code is\n"
             " formed as follows:\n"
             "   The code must be 3 letters, one each from the\n"
             "   pairs {R,L} {A,P} {I,S}.  The first letter gives\n"
             "   the orientation of the x-axis, the second the\n"
             "   orientation of the y-axis, the third the z-axis:\n"
             "      R = Right-to-Left          L = Left-to-Right\n"
             "      A = Anterior-to-Posterior  P = Posterior-to-Anterior\n"
             "      I = Inferior-to-Superior   S = Superior-to-Inferior\n"
             "   For example, 'LPI' means\n"
             "      -x = Left       +x = Right\n"
             "      -y = Posterior  +y = Anterior\n"
             "      -z = Inferior   +z = Superior\n"
             " * For display in SPM, 'LPI' or 'RPI' seem to work OK.\n"
             "    Be careful with this: you don't want to confuse L and R\n"
             "    in the SPM display!\n"
             " * If you DON'T use this option, the dataset will be written\n"
             "    out in the orientation in which it is stored in AFNI\n"
             "    (e.g., the output of '3dinfo dset' will tell you this.)\n"
             " * The dataset orientation is NOT stored in the .hdr file.\n"
             " * AFNI and ANALYZE data are stored in files with the x-axis\n"
             "    varying most rapidly and the z-axis most slowly.\n"
             " * Note that if you read an ANALYZE dataset into AFNI for\n"
             "    display, AFNI assumes the LPI orientation, unless you\n"
             "    set environment variable AFNI_ANALYZE_ORIENT.\n"
            ) ;
      PRINT_COMPILE_DATE; exit(0) ;
   }

   mainENTRY("3dAFNItoANALYZE main"); machdep(); PRINT_VERSION("3dAFNItoANALYZE");

   /*-- read inputs --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-4D") == 0 ){    /* 30 Sep 2002 */
       do_4D = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-orient") == 0 ){ /* 19 Mar 2003 */
       char acod ;

       if( iarg+1 >= argc ){
         fprintf(stderr,"** Need something after -orient!\n"); exit(1);
       }

       MCW_strncpy(orient_code,argv[++iarg],4) ;
       if( strlen(orient_code) != 3 ){
         fprintf(stderr,"** Illegal code '%s' after -orient!\n",argv[iarg]); exit(1);
       }

       acod = toupper(orient_code[0]) ; xxor = ORCODE(acod) ;
       acod = toupper(orient_code[1]) ; yyor = ORCODE(acod) ;
       acod = toupper(orient_code[2]) ; zzor = ORCODE(acod) ;

       if( xxor<0 || yyor<0 || zzor<0 || !OR3OK(xxor,yyor,zzor) ){
         fprintf(stderr,"** Unusable code after -orient!\n"); exit(1);
       }
       iarg++ ; continue ;
     }

     fprintf(stderr,"** Illegal option: %s\n",argv[iarg]); exit(1);
   }

   if( iarg >= argc-1 ){
     fprintf(stderr,"** Not enough arguments on command line!\n"); exit(1);
   }

   aname = argv[iarg++] ;
   if( !THD_filename_ok(aname) ){
     fprintf(stderr,"** Illegal aname string %s\n",aname) ;
     exit(1) ;
   }
   fname = malloc( strlen(aname)+16 ) ;

   dset = THD_open_dataset( argv[iarg++] ); CHECK_OPEN_ERROR(dset,argv[iarg-1]);

   if( xxor >= 0 ){  /* 19 Mar 2003: figure how to flip */
     xdir = THD_get_axis_direction( dset->daxes , xxor ) ;
     ydir = THD_get_axis_direction( dset->daxes , yyor ) ;
     zdir = THD_get_axis_direction( dset->daxes , zzor ) ;
     if(              ydir == 0 || zdir == 0 ) xdir = 0 ;
     if( xdir == 1 && ydir == 2 && zdir == 3 ) xdir = 0 ;
   }
   if( xdir != 0 ){
     float dx=fabs(DSET_DX(dset)) ,
           dy=fabs(DSET_DY(dset)) ,
           dz=fabs(DSET_DZ(dset))  ;
     DSET_mallocize(dset) ;
     switch( xdir ){
       case 1: case -1: xdel = dx ; break ;
       case 2: case -2: xdel = dy ; break ;
       case 3: case -3: xdel = dz ; break ;
     }
     switch( ydir ){
       case 1: case -1: ydel = dx ; break ;
       case 2: case -2: ydel = dy ; break ;
       case 3: case -3: ydel = dz ; break ;
     }
     switch( zdir ){
       case 1: case -1: zdel = dx ; break ;
       case 2: case -2: zdel = dy ; break ;
       case 3: case -3: zdel = dz ; break ;
     }
   } else {
     xdel = fabs(DSET_DX(dset)) ;
     ydel = fabs(DSET_DY(dset)) ;
     zdel = fabs(DSET_DZ(dset)) ;
   }

   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   /* determine if we scale to floats */

   scl = THD_need_brick_factor( dset ) ;

   /* 30 Sep 2002: if doing a 4D file, write single .hdr now */

   if( do_4D ){
     im = mri_empty_conforming( DSET_BRICK(dset,0) ,
                                (scl) ? MRI_float
                                      : DSET_BRICK_TYPE(dset,0) ) ;

     if( xdir != 0 ){
       qim = mri_flip3D( xdir,ydir,zdir , im ) ;
       if( qim == NULL){
         fprintf(stderr,"mri_flip3D fails?!\n"); exit(1);
       }
       mri_free(im); im = qim;
     }

     im->dx = xdel ;                    /* load voxel sizes */
     im->dy = ydel ;
     im->dz = zdel ;
     im->dw = 1.0 ;

     if( AFNI_yesenv("AFNI_ANALYZE_ORIGINATOR") ){
       im->xo = dset->daxes->xxorg ;                    /* load voxel origin */
       im->yo = dset->daxes->yyorg ;                    /* 03/11/04 KRH added this bit for SPM */
       im->zo = dset->daxes->zzorg ;
       if( ORIENT_sign[dset->daxes->xxorient] == '-' ){
         im->dx = -im->dx ;
         /* im->xo = -im->xo ; */
       }
       if( ORIENT_sign[dset->daxes->yyorient] == '-' ){
         im->dy = -im->dy ;
         /* im->yo = -im->yo ; */
       }
       if( ORIENT_sign[dset->daxes->zzorient] == '-' ){
         im->dz = -im->dz ;
         /* im->zo = -im->zo ; */
       }
     }

     im->nt = DSET_NVALS(dset) ;        /* add a time axis */
     im->dt = DSET_TR(dset) ;
     if( im->dt <= 0.0 ) im->dt = 1.0 ;
     if( DSET_TIMEUNITS(dset) == UNITS_MSEC_TYPE ) im->dt *= 0.001 ; /* 05 Jul 2005 */

     mri_write_analyze( aname , im ) ;  /* output 4D .hdr file */
     mri_free(im) ;

     sprintf(fname,"%s.img",aname) ;    /* open output .img file */
     ifp = fopen( fname , "wb" ) ;
     if( ifp == NULL ){
       fprintf(stderr,"** Can't open file %s for output!\n",fname) ;
       exit(1) ;
     }
   }

   /* loop over sub-bricks */

   for( ii=0 ; ii < DSET_NVALS(dset) ; ii++ ){

      im = DSET_BRICK(dset,ii) ;             /* get the sub-brick */

      if( scl ){                             /* scale it to floats */
        fac = DSET_BRICK_FACTOR(dset,ii) ;
        if( fac == 0.0 ) fac = 1.0 ;
        qim = mri_scale_to_float( fac , im ) ;
      } else {
        qim = im ;
      }

      if( xdir != 0 ){                       /* 19 Mar 2003: flip it */
        MRI_IMAGE *fim ;
        fim = mri_flip3D( xdir,ydir,zdir , qim ) ;
        if( fim == NULL ){
          fprintf(stderr,"mri_flip3D fails at ii=%d ?!\n",ii); exit(1);
        }
        if( qim != im ) mri_free(qim) ;
        qim = fim ;
      }

      if( do_4D ){                           /* 30 Sep 2002: write into 4D .img file */

        fwrite( mri_data_pointer(qim) , qim->nvox , qim->pixel_size , ifp ) ;

      } else {                               /* write separate 3D .hdr/.img files */

        qim->dx = xdel ;    /* load voxel sizes */
        qim->dy = ydel ;
        qim->dz = zdel ;
        qim->dw = 1.0 ;

        if( AFNI_yesenv("AFNI_ANALYZE_ORIGINATOR") ){
          qim->xo = dset->daxes->xxorg ;                    /* load voxel origin */
          qim->yo = dset->daxes->yyorg ;                    /* 03/11/04 KRH added this bit for SPM */
          qim->zo = dset->daxes->zzorg ;

          if( ORIENT_sign[dset->daxes->xxorient] == '-' ){
            qim->dx   = -qim->dx   ;
            /* qim->xo = -qim->xo ; */
          }
          if( ORIENT_sign[dset->daxes->yyorient] == '-' ){
            qim->dy   = -qim->dy   ;
            /* qim->yo = -qim->yo ; */
          }
          if( ORIENT_sign[dset->daxes->zzorient] == '-' ){
            qim->dz   = -qim->dz   ;
            /* qim->zo = -qim->zo ; */
          }
        }

        sprintf(fname,"%s_%04d",aname,ii) ;  /* make up a filename */
        mri_write_analyze( fname , qim ) ;   /* do the real work */
      }

      if( qim != im ) mri_free(qim) ;
      DSET_unload_one(dset,ii) ;             /* clean up the trash */
   }

   if( ifp != NULL ) fclose(ifp) ;           /* 30 Sep 2002 */

   free(fname) ; exit(0) ;
}
