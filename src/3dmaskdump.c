/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

extern int *z_rand_order(int bot, int top, long int seed);

/*----------------
  Another quickie.
------------------*/

int main( int argc , char * argv[] )
{
   int narg , nvox , ii,jj,kk,vv , mcount , iv , mc , ndset,ndval , i,j,k ;
   THD_3dim_dataset *mask_dset=NULL , **input_dset=NULL ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   byte *mmm   = NULL ;
   char *oname = NULL , *obuf , *otemp ;
   FILE *ofile ;
   MRI_IMAGE *flim ;
   float *flar ;
   int no_ijk=0 , yes_xyz=0 ;
   int yes_index=0 ;                    /*-- 09 May 2003 [rickr] --*/
   byte *cmask=NULL ; int ncmask=0 ;
   int verb=1 ;
   int yes_niml=0 , no_zero=0 , numz ;   /* 04 Feb 2008 */
   int nout ; char **bout , *niml_name="maskdump" ;
   int nrand = -1;
   byte *bmask = NULL ;      /*-- box+ball mask: moved here 09 Sep 2009 --*/

   int box_num=0 ; float *box_dat=NULL ;   /* 09 May 2003 - RWCox */
   int ball_num=0; float *ball_dat=NULL;   /* 09 Sep 2009 - RWCox */
   int nx,ny,nz,nxy,nxyz ;
   unsigned int nrandseed = 1234u;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
"Usage: 3dmaskdump [options] dataset dataset ...\n"
 "Writes to an ASCII file values from the input datasets\n"
 "which satisfy the mask criteria given in the options.\n"
 "If no options are given, then all voxels are included.\n"
 "This might result in a GIGANTIC output file.\n"
 "Options:\n"
 "  -mask mset   Means to use the dataset 'mset' as a mask:\n"
 "                 Only voxels with nonzero values in 'mset'\n"
 "                 will be printed from 'dataset'.  Note\n"
 "                 that the mask dataset and the input dataset\n"
 "                 must have the same number of voxels.\n"
 "  -mrange a b  Means to further restrict the voxels from\n"
 "                 'mset' so that only those mask values\n"
 "                 between 'a' and 'b' (inclusive) will\n"
 "                 be used.  If this option is not given,\n"
 "                 all nonzero values from 'mset' are used.\n"
 "                 Note that if a voxel is zero in 'mset', then\n"
 "                 it won't be included, even if a < 0 < b.\n"
/*-- 09 May 2003: add -index option [rickr] */
 "  -index       Means to write out the dataset index values.\n"
 "  -noijk       Means not to write out the i,j,k values.\n"
 "  -xyz         Means to write the x,y,z coordinates from\n"
 "                 the 1st input dataset at the start of each\n"
 "                 output line.  These coordinates are in\n"
 "                 the 'RAI' (DICOM) order.\n"
 "  -o fname     Means to write output to file 'fname'.\n"
 "                 [default = stdout, which you won't like]\n"
 "\n"
 "  -cmask 'opts' Means to execute the options enclosed in single\n"
 "                  quotes as a 3dcalc-like program, and produce\n"
 "                  produce a mask from the resulting 3D brick.\n"
 "       Examples:\n"
 "        -cmask '-a fred+orig[7] -b zork+orig[3] -expr step(a-b)'\n"
 "                  produces a mask that is nonzero only where\n"
 "                  the 7th sub-brick of fred+orig is larger than\n"
 "                  the 3rd sub-brick of zork+orig.\n"
 "        -cmask '-a fred+orig -expr 1-bool(k-7)'\n"
 "                  produces a mask that is nonzero only in the\n"
 "                  7th slice (k=7); combined with -mask, you\n"
 "                  could use this to extract just selected voxels\n"
 "                  from particular slice(s).\n"
 "       Notes: * You can use both -mask and -cmask in the same\n"
 "                  run - in this case, only voxels present in\n"
 "                  both masks will be dumped.\n"
 "              * Only single sub-brick calculations can be\n"
 "                  used in the 3dcalc-like calculations -\n"
 "                  if you input a multi-brick dataset here,\n"
 "                  without using a sub-brick index, then only\n"
 "                  its 0th sub-brick will be used.\n"
 "              * Do not use quotes inside the 'opts' string!\n"
 "\n"
 "  -xbox x y z   Means to put a 'mask' down at the dataset (not DICOM)\n"
 "                  coordinates of 'x y z' mm.  By default, this box is\n"
 "                  1 voxel wide in each direction.  You can specify\n"
 "                  instead a range of coordinates using a colon ':'\n"
 "                  after the coordinates; for example:\n"
 "                    -xbox 22:27 31:33 44\n"
 "                  means a box from (x,y,z)=(22,31,44) to (27,33,44).\n"
 "           NOTE: dataset coordinates are NOT the coordinates you\n"
 "                 typically see in AFNI's main controller top left corner.\n"
 "                 Those coordinates are typically in either RAI/DICOM order\n"
 "                 or in LPI/SPM order and should be used with -dbox and\n"
 "                 -nbox, respectively.\n"
 "\n"
 "  -dbox x y z   Means the same as -xbox, but the coordinates are in\n"
 "                  RAI/DICOM order (+x=Left, +y=Posterior, +z=Superior).\n"
 "                  If your AFNI environment variable AFNI_ORIENT is set to\n"
 "                  RAI, these coordinates correspond to those you'd enter\n"
 "                  into the 'Jump to (xyz)' control in AFNI, and to\n"
 "                  those output by 3dclust.\n"
 "            NOTE: It is possible to make AFNI and/or 3dclust output \n"
 "                  coordinates in an order different from the one specified \n"
 "                  by AFNI_ORIENT, but you'd have to work hard on that. \n"
 "                  In any case, the order is almost always specified along \n"
 "                  with the coordinates. If you see RAI/DICOM, then use \n"
 "                  -dbox. If you see LPI/SPM then use -nbox. \n"
 "\n"
 "  -nbox x y z   Means the same as -xbox, but the coordinates are in\n"
 "                  LPI/SPM or 'neuroscience' order where the signs of the\n"
 "                  x and y coordinates are reversed relative to RAI/DICOM.\n"
 "                  (+x=Right, +y=Anterior, +z=Superior)\n"
 "\n"
 "  -ibox i j k   Means to put a 'mask' down at the voxel indexes\n"
 "                  given by 'i j k'.  By default, this picks out\n"
 "                  just 1 voxel.  Again, you can use a ':' to specify\n"
 "                  a range (now in voxels) of locations.\n"
 "       Notes: * Boxes are cumulative; that is, if you specify more\n"
 "                  than 1 box, you'll get more than one region.\n"
 "              * If a -mask and/or -cmask option is used, then\n"
 "                  the INTERSECTION of the boxes with these masks\n"
 "                  determines which voxels are output; that is,\n"
 "                  a voxel must be inside some box AND inside the\n"
 "                  mask in order to be selected for output.\n"
 "              * If boxes select more than 1 voxel, the output lines\n"
 "                  are NOT necessarily in the order of the options on\n"
 "                  the command line.\n"
 "              * Coordinates (for -xbox, -dbox, and -nbox) are relative\n"
 "                  to the first dataset on the command line.\n"
 "\n"
 "  -xball x y z r  Means to put a ball (sphere) mask down at dataset\n"
 "                    coordinates (x,y,z) with radius r.\n"
 "  -dball x y z r  Same, but (x,y,z) are in RAI/DICOM order.\n"
 "  -nball x y z r  Same, but (x,y,z) are in LPI/SPM order.\n"
 "       Notes: * The combined (set UNION) of all ball and/or box masks\n"
 "                is created first.  Then, if a -mask and/or -cmask\n"
 "                option was used, then the ball+box mask will be\n"
 "                INTERSECTED with the existing mask.\n"
 "\n"
 "  -nozero       Means to skip output of any voxel where all the\n"
 "                  data values are zero.\n"
 "\n"
 "  -n_rand N_RAND Means to keep only N_RAND randomly selected\n"
 "                 voxels from what would have been the output.\n"
 "\n"
 "  -n_randseed SEED  Seed the random number generator with SEED,\n"
 "                    instead of the default seed of %u\n"
 "\n"
 "  -niml name    Means to output data in the XML/NIML format that\n"
 "                  is compatible with input back to AFNI via\n"
 "                  the READ_NIML_FILE command.\n"
 "              * 'name' is the 'target_name' for the NIML header\n"
 "                  field, which is the name that will be assigned\n"
 "                  to the dataset when it is sent into AFNI.\n"
 "              * Also implies '-noijk' and '-xyz' and '-nozero'.\n"
 "\n"
 "  -quiet        Means not to print progress messages to stderr.\n"
 "\n"
 "Inputs after the last option are datasets whose values you\n"
 "want to be dumped out.  These datasets (and the mask) can\n"
 "use the sub-brick selection mechanism (described in the\n"
 "output of '3dcalc -help') to choose which values you get.\n"
 "\n"
 "Each selected voxel gets one line of output:\n"
 "  i j k val val val ....\n"
 "where (i,j,k) = 3D index of voxel in the dataset arrays,\n"
 "and val = the actual voxel value.  Note that if you want\n"
 "the mask value to be output, you have to include that\n"
 "dataset in the dataset input list again, after you use\n"
 "it in the '-mask' option.\n"
 "\n"
 "* To eliminate the 'i j k' columns, use the '-noijk' option.\n"
 "* To add spatial coordinate columns, use the '-xyz' option.\n"
 "\n"
 "N.B.: This program doesn't work with complex-valued datasets!\n"
            , nrandseed) ;

      printf("\n" MASTER_SHORTHELP_STRING ) ;

      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dmaskdump main"); machdep() ;


   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3dmaskdump",argc,argv) ;

   /* scan argument list */

   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

      if( strcasecmp(argv[narg],"-niml") == 0 ){             /* 04 Feb 2008 */
        yes_niml = 1 ; narg++ ;
        if( narg >= argc ) ERROR_exit("-niml must be followed by a name") ;
        niml_name = strdup(argv[narg]) ;
        if( !THD_filename_pure(niml_name) )
          ERROR_exit("Illegal 'name' after '-niml'") ;
        narg++ ; continue ;
      }

      if( strcasecmp(argv[narg],"-nozero") == 0 ){           /* 04 Feb 2008 */
        no_zero = 1 ; narg++ ; continue ;
      }


      if( strcmp(argv[narg],"-quiet") == 0 ){            /* 09 May 2003 - RWC */
        verb = 0 ; narg++ ; continue ;
      }

      if( strcmp(argv[narg]+2,"box") == 0 ){             /* 09 May 2003 - RWC */
        float xbot,xtop , ybot,ytop , zbot,ztop , btyp ;
        int nn ;
        char code = *(argv[narg]+1) ;   /* should be 'x', 'd' , 'n', or 'i' */
        switch( code ){
          case 'x': btyp = BOX_XYZ ; break ;
          case 'd': btyp = BOX_DIC ; break ;
          case 'n': btyp = BOX_NEU ; break ;
          case 'i': btyp = BOX_IJK ; break ;
          default:  ERROR_exit("Unknown 'box' option %s\n",argv[narg]) ;
        }
        if( narg+3 >= argc )
          ERROR_exit("need 3 arguments after %s\n",argv[narg]);
        nn = sscanf( argv[narg+1] , "%f:%f" , &xbot , &xtop ) ;
        if( nn < 1 )
          ERROR_exit("Can't decode %s after %s\n",argv[narg+1],argv[narg]);
        else if( nn == 1 )
          xtop=xbot ;
        nn = sscanf( argv[narg+2] , "%f:%f" , &ybot , &ytop ) ;
        if( nn < 1 )
          ERROR_exit("Can't decode %s after %s\n",argv[narg+2],argv[narg]);
        else if( nn == 1 )
          ytop=ybot ;
        nn = sscanf( argv[narg+3] , "%f:%f" , &zbot , &ztop ) ;
        if( nn < 1 )
          ERROR_exit("Can't decode %s after %s\n",argv[narg+3],argv[narg]);
        else if( nn == 1 )
          ztop=zbot ;
        box_dat = (float *) realloc( box_dat , sizeof(float)*BOXLEN*(box_num+1) ) ;
        box_dat[0+BOXLEN*box_num] = btyp ;
        box_dat[1+BOXLEN*box_num] = xbot ;
        box_dat[2+BOXLEN*box_num] = xtop ;
        box_dat[3+BOXLEN*box_num] = ybot ;
        box_dat[4+BOXLEN*box_num] = ytop ;
        box_dat[5+BOXLEN*box_num] = zbot ;
        box_dat[6+BOXLEN*box_num] = ztop ;
        box_num++ ; narg += 4 ; continue ;
      }

      if( strcmp(argv[narg]+2,"ball") == 0 ){            /* 09 Sep 2009 - RWC */
        float xcen,ycen,zcen,rad , btyp ;
        char code = *(argv[narg]+1) ;   /* should be 'x', 'd' , or 'n' */
        switch( code ){
          case 'x': btyp = BALL_XYZ ; break ;
          case 'd': btyp = BALL_DIC ; break ;
          case 'n': btyp = BALL_NEU ; break ;
          default:  ERROR_exit("Unknown 'ball' option %s",argv[narg]) ;
        }
        if( narg+4 >= argc )
          ERROR_exit("need 4 arguments after %s\n",argv[narg]);
        xcen = strtod( argv[++narg] , NULL ) ;
        ycen = strtod( argv[++narg] , NULL ) ;
        zcen = strtod( argv[++narg] , NULL ) ;
        rad  = strtod( argv[++narg] , NULL ) ;
        if( rad <= 0.0f ){
          WARNING_message("%s radius=%s !?",argv[narg-4],argv[narg]) ; rad = 0.0f;
        }

        ball_dat = (float *) realloc( ball_dat , sizeof(float)*BOXLEN*(ball_num+1) ) ;
        ball_dat[0+BOXLEN*ball_num] = btyp ;
        ball_dat[1+BOXLEN*ball_num] = xcen ;
        ball_dat[2+BOXLEN*ball_num] = ycen ;
        ball_dat[3+BOXLEN*ball_num] = zcen ;
        ball_dat[4+BOXLEN*ball_num] = rad  ;
        ball_num++ ; narg++ ;
        continue ;
      }

      /*-- 09 May 2003: option to output index value (for Mike B) [rickr] --*/

      if( strcmp(argv[narg],"-index") == 0 ){
        yes_index = 1 ;
        narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-noijk") == 0 ){
         no_ijk = 1 ;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-xyz") == 0 ){   /* 23 Mar 2003 */
         yes_xyz = 1 ;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-cmask") == 0 ){  /* 16 Mar 2000 */
         if( narg+1 >= argc )
            ERROR_exit("-cmask option requires a following argument!\n");
         cmask = EDT_calcmask( argv[++narg] , &ncmask, 0 ) ;
         if( cmask == NULL ) ERROR_exit("Can't compute -cmask!\n");
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL )
           ERROR_exit("Cannot have two -mask options!\n") ;
         if( narg+1 >= argc )
           ERROR_exit("-mask option requires a following argument!\n");
         mask_dset = THD_open_dataset( argv[++narg] ) ;
         if( mask_dset == NULL )
           ERROR_exit("Cannot open mask dataset!\n") ;
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex )
           ERROR_exit("Cannot deal with complex-valued mask dataset!\n");
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mrange",5) == 0 ){
         if( narg+2 >= argc )
           ERROR_exit("-mrange option requires 2 following arguments!\n");
         mask_bot = strtod( argv[++narg] , NULL ) ;
         mask_top = strtod( argv[++narg] , NULL ) ;
         if( mask_top < mask_top )
           ERROR_exit("-mrange inputs are illegal!\n") ;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-n_rand") == 0 ){
         if( narg+1 >= argc )
           ERROR_exit("-n_rand option requires 1 argument!\n");
         nrand = (int)strtod( argv[++narg] , NULL ) ;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-n_randseed") == 0 ){
         double temp ;
         if( narg+1 >= argc )
           ERROR_exit("-n_randseed option requires 1 argument!\n");
         temp = strtod( argv[++narg] , NULL ) ;
         nrandseed = temp ;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-o") == 0 ){
         if( narg+1 >= argc )
           ERROR_exit("-o needs an argument after it!\n");
         oname = argv[++narg] ;
         if( ! THD_filename_ok(oname) )
           ERROR_exit("name after -o is illegal!\n");
         if( THD_is_file(oname) )
           ERROR_exit("file %s already exists!\n",oname);
         narg++ ; continue ;
      }

      ERROR_exit("Unknown option: %s\n",argv[narg]) ;
   }

   if( yes_niml ){
     no_ijk = 1; yes_xyz = 1; yes_index = 0; no_zero = 1; /* 04 Feb 2008 */
   }

   /* keep -quiet quiet                      27 Mar 2006 [rickr] */
   if( verb > 0 ) PRINT_VERSION("3dmaskdump") ;

   /* should have at least one more argument */

   ndset = argc - narg ;
   if( ndset <= 0 ) ERROR_exit("No input dataset!?\n") ;

   /* open all input datasets */

   input_dset = (THD_3dim_dataset **) malloc( sizeof(THD_3dim_dataset *)*ndset ) ;
   for( ndval=ii=0 ; ii < ndset ; ii++ ){
      input_dset[ii] = THD_open_dataset( argv[narg+ii] ) ;
      if( input_dset[ii] == NULL )
        ERROR_exit("Can't open dataset %s!\n",argv[narg+ii]) ;

      if( DSET_BRICK_TYPE(input_dset[ii],0) == MRI_complex )
        ERROR_exit("Cannot deal with complex-valued input dataset!\n");

      if( ii == 0 ){
        nvox = DSET_NVOX(input_dset[0]) ;
        nx   = DSET_NX(input_dset[0]) ;
        ny   = DSET_NY(input_dset[0]) ; nxy  = nx*ny ;
        nz   = DSET_NZ(input_dset[0]) ; nxyz = nxy*nz ;
      } else if( DSET_NX(input_dset[ii]) != nx ||
                 DSET_NY(input_dset[ii]) != ny || DSET_NZ(input_dset[ii]) != nz ){
        ERROR_exit("Dataset %s does not match in size!\n",argv[narg+ii]);
      }

      ndval += DSET_NVALS(input_dset[ii]) ;
   }

   /* make a byte mask from mask dataset */

   if( mask_dset == NULL ){
      mmm = NULL ;
      if( verb ) INFO_message("%d voxels in the entire dataset (no mask)\n",nvox) ;
   } else {
      if( DSET_NVOX(mask_dset) != nvox )
        ERROR_exit("Input and mask datasets are not same dimensions!\n");
      mmm = THD_makemask( mask_dset , 0 , mask_bot,mask_top ) ;
      mcount = THD_countmask( nvox , mmm ) ;
      if( mcount <= 0 ) ERROR_exit("No voxels in the mask!\n") ;
      if( verb ) INFO_message("%d voxels in the mask\n",mcount) ;
      DSET_delete(mask_dset) ;
   }

   /* 16 Mar 2000: deal with the cmask */

   if( cmask != NULL ){
      if( ncmask != nvox )
        ERROR_exit("Input and cmask datasets are not same dimensions!\n");
      if( mmm != NULL ){
         for( ii=0 ; ii < nvox ; ii++ ) mmm[ii] = (mmm[ii] && cmask[ii]) ;
         free(cmask) ;
         mcount = THD_countmask( nvox , mmm ) ;
         if( mcount <= 0 ) ERROR_exit("No voxels in the mask+cmask!\n") ;
         if( verb ) INFO_message("%d voxels in the mask+cmask\n",mcount) ;
      } else {
         mmm = cmask ;
         mcount = THD_countmask( nvox , mmm ) ;
         if( mcount <= 0 ) ERROR_exit("No voxels in the cmask!\n") ;
         if( verb ) INFO_message("%d voxels in the cmask\n",mcount) ;
      }
   }

   /*-- 09 May 2003: make a mask corresponding to the boxen - RWC --*/

   if( box_num > 0 ){
     int bb, ibot,itop, jbot,jtop, kbot,ktop , btyp , ii,jj,kk ;
     float   xbot,xtop, ybot,ytop, zbot,ztop ;
     THD_fvec3 dv,xv ;
     THD_3dim_dataset *dset = input_dset[0] ;
     float xmin=dset->daxes->xxmin , xmax=dset->daxes->xxmax ;
     float ymin=dset->daxes->yymin , ymax=dset->daxes->yymax ;
     float zmin=dset->daxes->zzmin , zmax=dset->daxes->zzmax ;

     if( bmask == NULL ) bmask = calloc(1,nvox) ;

     for( bb=0 ; bb < box_num ; bb++ ){
       btyp = box_dat[0+BOXLEN*bb];
       xbot = box_dat[1+BOXLEN*bb]; xtop = box_dat[2+BOXLEN*bb];
       ybot = box_dat[3+BOXLEN*bb]; ytop = box_dat[4+BOXLEN*bb];
       zbot = box_dat[5+BOXLEN*bb]; ztop = box_dat[6+BOXLEN*bb];

       if( btyp != BOX_IJK ){                      /* convert coords to indexes */

         if( btyp == BOX_NEU ){                    /* coords from Neuroscience to DICOM */
           xbot = -xbot; xtop = -xtop; ybot = -ybot; ytop = -ytop; btyp = BOX_DIC;
         }
         if( btyp == BOX_DIC ){                    /* coords from DICOM to dataset */
           LOAD_FVEC3(dv,xbot,ybot,zbot) ;
           xv = THD_dicomm_to_3dmm( dset , dv ) ;
           UNLOAD_FVEC3(xv,xbot,ybot,zbot) ;
           LOAD_FVEC3(dv,xtop,ytop,ztop) ;
           xv = THD_dicomm_to_3dmm( dset , dv ) ;
           UNLOAD_FVEC3(xv,xtop,ytop,ztop) ;
         }
         if( xbot < xmin && xtop < xmin ) continue ; /* skip box if outside dataset */
         if( xbot > xmax && xtop > xmax ) continue ;
         if( ybot < ymin && ytop < ymin ) continue ;
         if( ybot > ymax && ytop > ymax ) continue ;
         if( zbot < zmin && ztop < zmin ) continue ;
         if( zbot > zmax && ztop > zmax ) continue ;
         LOAD_FVEC3(dv,xbot,ybot,zbot) ;
         xv = THD_3dmm_to_3dfind( dset , dv ) ;   /* coords from dataset to index */
         UNLOAD_FVEC3(xv,xbot,ybot,zbot) ;
         LOAD_FVEC3(dv,xtop,ytop,ztop) ;
         xv = THD_3dmm_to_3dfind( dset , dv ) ;
         UNLOAD_FVEC3(xv,xtop,ytop,ztop) ;
       }
       ibot = rint(xbot) ; jbot = rint(ybot) ; kbot = rint(zbot) ;  /* round */
       itop = rint(xtop) ; jtop = rint(ytop) ; ktop = rint(ztop) ;
       if( ibot > itop ){ btyp = ibot; ibot = itop; itop = btyp; }  /* flip? */
       if( jbot > jtop ){ btyp = jbot; jbot = jtop; jtop = btyp; }
       if( kbot > ktop ){ btyp = kbot; kbot = ktop; ktop = btyp; }

       /* skip box if outside dataset */
       if ( itop < 0 || ibot >= nx ) continue;
       if ( jtop < 0 || jbot >= ny ) continue;
       if ( ktop < 0 || kbot >= nz ) continue;

       /* constrain values to dataset dimensions */
       if ( ibot < 0 ) ibot = 0;  if ( itop >= nx ) itop = nx-1;
       if ( jbot < 0 ) jbot = 0;  if ( jtop >= ny ) jtop = ny-1;
       if ( kbot < 0 ) kbot = 0;  if ( ktop >= nz ) ktop = nz-1;

       for( kk=kbot ; kk <= ktop ; kk++ )
        for( jj=jbot ; jj <= jtop ; jj++ )
         for( ii=ibot ; ii <= itop ; ii++ ) bmask[ii+jj*nx+kk*nxy] = 1 ;

     } /* end of loop over list of boxes */

   } /* end of box mask */

   /*-- 09 Sep 2009: make a mask corresponding to the balls - RWC --*/

   if( ball_num > 0 ){
     int bb, ibot,itop, jbot,jtop, kbot,ktop , btyp , ii,jj,kk ;
     float   xcen,ycen,zcen , rad , xx,yy,zz , dist , icen,jcen,kcen ;
     THD_fvec3 dv,xv ;
     THD_3dim_dataset *dset = input_dset[0] ;
     float xmin=dset->daxes->xxmin , xmax=dset->daxes->xxmax ;
     float ymin=dset->daxes->yymin , ymax=dset->daxes->yymax ;
     float zmin=dset->daxes->zzmin , zmax=dset->daxes->zzmax ;

     if( bmask == NULL ) bmask = calloc(1,nvox) ;  /* will include boxes */

     /* loop over list of balls */

     for( bb=0 ; bb < ball_num ; bb++ ){
       btyp = ball_dat[0+BOXLEN*bb] ;
       xcen = ball_dat[1+BOXLEN*bb] ; ycen = ball_dat[2+BOXLEN*bb] ;
       zcen = ball_dat[3+BOXLEN*bb] ; rad  = ball_dat[4+BOXLEN*bb] ;

       /* convert center coords to dataset indexes */

       if( btyp == BALL_NEU ){          /* coords from Neuroscience to DICOM */
         xcen = -xcen; ycen = -ycen; btyp = BALL_DIC;
       }
       if( btyp == BALL_DIC ){          /* coords from DICOM to dataset */
         LOAD_FVEC3(dv,xcen,ycen,zcen) ;
         xv = THD_dicomm_to_3dmm( dset , dv ) ;
         UNLOAD_FVEC3(xv,xcen,ycen,zcen) ;
       }
       if( xcen < xmin || xcen > xmax ) continue ;  /* skip ball if outside */
       if( ycen < ymin || ycen > ymax ) continue ;
       if( zcen < zmin || zcen > zmax ) continue ;
       LOAD_FVEC3(dv,xcen,ycen,zcen) ;
       xv = THD_3dmm_to_3dfind( dset , dv ) ;   /* coords from dataset to index */
       UNLOAD_FVEC3(xv,icen,jcen,kcen) ;

       ibot = rint(icen-rad) ; itop = rint(icen+rad) ; /* box around ball */
       jbot = rint(jcen-rad) ; jtop = rint(jcen+rad) ;
       kbot = rint(kcen-rad) ; ktop = rint(kcen+rad) ;

       rad = rad*rad ;

       for( kk=kbot ; kk <= ktop ; kk++ ){
        for( jj=jbot ; jj <= jtop ; jj++ ){
         for( ii=ibot ; ii <= itop ; ii++ ){
            LOAD_FVEC3( dv , ii,jj,kk ) ;          /* convert to xyz coords */
            xv = THD_3dfind_to_3dmm( dset , dv ) ; /* then test distance^2 */
            UNLOAD_FVEC3( xv , xx,yy,zz ) ;        /* xyz of ball center. */
            dist = SQR(xx-xcen) + SQR(yy-ycen) + SQR(zz-zcen) ;
            if( dist <= rad ) bmask[ii+jj*nx+kk*nxy] = 1 ;
       }}}
     } /* end of loop over list of balls */

   } /* end of ball maskology */

   /*--- intersect boxes+balls mask with any other mask we have now ---*/

   if( bmask != NULL ){
     mcount = THD_countmask( nvox , bmask ) ;
     if( verb ) INFO_message("%d voxels in the boxes and/or balls\n",mcount) ;
     if( mcount == 0 )
       ERROR_exit("Can't continue with no voxels insides boxes+balls!\n");
     if( mmm != NULL ){
       for( ii=0 ; ii < nvox ; ii++ )
          mmm[ii] = (mmm[ii] && bmask[ii]) ;  /* intersection */
       free(bmask) ;
       mcount = THD_countmask( nvox , mmm ) ;
       if( mcount <= 0 ) ERROR_exit("No voxels in the mask INTERSECT boxes+balls!\n") ;
       if( verb ) INFO_message("%d voxels in the mask INTERSECT boxes+balls\n",mcount) ;
     } else {
       mmm = bmask ;
       if( verb ) INFO_message("Using only the boxes+balls mask") ;
     }
   }

   /*----- read in input dataset bricks -----*/

   for( ii=0 ; ii < ndset ; ii++ ){
     DSET_load(input_dset[ii]) ;  CHECK_LOAD_ERROR(input_dset[ii]) ;
   }

   /*--- open the output file ---*/

   if( oname != NULL ){
     ofile = fopen( oname , "w" ) ;
     if( ofile == NULL ) ERROR_exit("Can't open output file %s\n",oname);
   } else {
     ofile = stdout ;
   }

   /*--- output string buffers ---*/

   /*-- 09 May 2003: add room for the index (9 -> 10)    [rickr] --*/
   obuf = (char *) malloc( sizeof(char) * (ndval+10) * 16 ) ;

   /* do we want a random set? */
   if (nrand > 0) {
      int *iok = (int *)calloc(nvox, sizeof(int));
      int *ranorder=NULL;
      int cnt=0 , ccc ;

      cnt = 0;
      for( ii=0 ; ii < nvox ; ii++ ){
         if( mmm == NULL || mmm[ii]) {
            iok[cnt] = ii;
            ++cnt;
         }
      }
      if( verb )
         INFO_message("Dumping %d randomly selected voxels \n"
                      "out of %d originally in the mask.\n",
                      ( (cnt<nrand) ? cnt : nrand ), cnt);
      ccc = cnt-1 ;
      /* z_rand_order line causes a compiler error on macosx_10.3_G5
       *
       * 3dmaskdump.c: In function `main':
       * 3dmaskdump.c:723: error: unrecognizable insn:
       * ...
       * 3dmaskdump.c:723: internal compiler error: in extract_insn,
       *                   at recog.c:2175
       * Old OS and system, no bug report filed.   15 Oct 2009 [rickr]
       */
      ranorder = z_rand_order(0, ccc, nrandseed);
      if (mmm) free(mmm); mmm = (byte *)calloc(nvox, sizeof(byte));
      for (ii=0; ii < ( (cnt<nrand) ? cnt : nrand ); ++ii) {
         mmm[iok[ranorder[ii]]] = 1;
      }

      free(iok); iok = NULL;
      free(ranorder); ranorder = NULL;
   }

   /*------- loop over voxels -------*/

   nout = 0 ; bout = NULL ;
   for( ii=0 ; ii < nvox ; ii++ ){
      if( mmm != NULL && mmm[ii] == 0 ) continue ;  /* skip this voxel */

      obuf[0] = '\0' ; /* start it off with nothing */

      /*-- 09 May 2003: optionally display index    [rickr] --*/
      if ( yes_index ){
        otemp = MV_format_fval((float)ii);
        strcat(obuf,otemp); strcat(obuf," ");
      }

      if( ! no_ijk ){
        i = DSET_index_to_ix( input_dset[0] , ii ) ;  /* voxel indexes */
        j = DSET_index_to_jy( input_dset[0] , ii ) ;
        k = DSET_index_to_kz( input_dset[0] , ii ) ;

        otemp = MV_format_fval((float)i); strcat(obuf,otemp); strcat(obuf," ");
        otemp = MV_format_fval((float)j); strcat(obuf,otemp); strcat(obuf," ");
        otemp = MV_format_fval((float)k); strcat(obuf,otemp); strcat(obuf," ");
      }

      if( yes_xyz ){                                  /* 23 Mar 2003 */
        THD_ivec3 ind ; THD_fvec3 vec ;
        i = DSET_index_to_ix( input_dset[0] , ii ) ;
        j = DSET_index_to_jy( input_dset[0] , ii ) ;
        k = DSET_index_to_kz( input_dset[0] , ii ) ;
        LOAD_IVEC3(ind,i,j,k) ;
        vec = THD_3dind_to_3dmm ( input_dset[0] , ind ) ;
        vec = THD_3dmm_to_dicomm( input_dset[0] , vec ) ;
        otemp = MV_format_fval(vec.xyz[0]); strcat(obuf,otemp); strcat(obuf," ");
        otemp = MV_format_fval(vec.xyz[1]); strcat(obuf,otemp); strcat(obuf," ");
        otemp = MV_format_fval(vec.xyz[2]); strcat(obuf,otemp); strcat(obuf," ");
      }

      for( numz=kk=0 ; kk < ndset ; kk++ ){
        flim = THD_extract_series( ii , input_dset[kk] , 0 ) ;
        flar = MRI_FLOAT_PTR(flim) ;
        for( vv=0 ; vv < flim->nx ; vv++ ){
           if( flar[vv] == 0.0f ) numz++ ;         /* 04 Feb 2008 */
           otemp = MV_format_fval(flar[vv]) ;
           strcat(obuf,otemp) ; strcat(obuf," ") ;
        }
        mri_free(flim) ;
      }
      if( no_zero && numz == ndval ) continue ;     /* 04 Feb 2008 */

      jj = strlen(obuf) ; obuf[jj-1] = '\0' ; /* kill last blank */

      if( yes_niml ){           /* 04 Feb 2008: save output for NIML-ization */
        nout++ ;
        bout = (char **)realloc(bout,sizeof(char *)*nout) ;
        bout[nout-1] = strdup(obuf) ;
      } else {
        fprintf(ofile,"%s\n",obuf) ;  /* regular output */
      }

   } /*--- end of loop over voxels ---*/

   if( yes_niml ){  /*----- 04 Feb 2008: NIML formatted output -----*/
     if( nout > 0 ){
       char *gstr = EDIT_get_geometry_string(input_dset[0]) ;
       fprintf(ofile,"<VOLUME_DATA_SPARSE\n") ;
       if( gstr != NULL && *gstr != '\0' )
         fprintf(ofile,"  geometry_string='%s'\n",gstr) ;
       fprintf(ofile,"  target_name='%s'\n",niml_name) ;
       fprintf(ofile,"  ni_type='%d*float'\n",ndval+3) ; /* +3 allows for xyz */
       fprintf(ofile,"  ni_dimen='%d'\n",nout) ;
       fprintf(ofile,">\n") ;
       for( ii=0 ; ii < nout ; ii++ ){
         fprintf(ofile,"%s\n",bout[ii]); free(bout[ii]);
       }
       fprintf(ofile,"</VOLUME_DATA_SPARSE>\n") ;
       free(bout) ;
       if( verb ) INFO_message("-niml: output %d voxels",nout) ;
     } else {
       ERROR_exit("-niml output requested, but no data to output!") ;
     }
   }

   /*----- Free at last! -----*/

   fflush(ofile); fsync(fileno(ofile)); fclose(ofile); exit(0) ;
}
