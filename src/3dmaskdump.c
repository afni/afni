/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*----------------
  Another quickie.
------------------*/

int main( int argc , char * argv[] )
{
   int narg , nvox , ii,jj,kk,vv , mcount , iv , mc , ndset,ndval , i,j,k ;
   THD_3dim_dataset *mask_dset=NULL , **input_dset=NULL ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   byte * mmm   = NULL ;
   char * oname = NULL , * obuf , * otemp ;
   FILE * ofile ;
   MRI_IMAGE * flim ;
   float * flar ;
   int noijk=0 , yes_xyz=0 ;
   int yes_index=0 ;                    /*-- 09 May 2003 [rickr] --*/
   byte *cmask=NULL ; int ncmask=0 ;
   int verb=1 ;

#define BOXLEN   7
#define BOX_XYZ  1
#define BOX_DIC  2
#define BOX_NEU  3
#define BOX_IJK  4
   int box_num=0 ; float *box_dat=NULL ;   /* 09 May 2003 */
   int nx,ny,nz,nxy,nxyz ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dmaskdump [options] dataset dataset ...\n"
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
             "                 the 'RAI' order.\n"
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
             "\n"
             "  -dbox x y z   Means the same as -xbox, but the coordinates are in\n"
             "                  DICOM order (+x=Left, +y=Posterior, +z=Superior).\n"
             "                  These coordinates correspond to those you'd enter\n"
             "                  into the 'Jump to (xyz)' control in AFNI, and to\n"
             "                  those output by default from 3dclust.\n"
             "  -nbox x y z   Means the same as -xbot, but the coordinates are in\n"
             "                  'neuroscience' order (+x=Right, +y=Anterior, +z=Superior)\n"
             "\n"
             "  -ibox i j k   Means to put a 'mask' down at the voxel indexes\n"
             "                  given by 'i j k'.  By default, this picks out\n"
             "                  just 1 voxel.  Again, you can use a ':' to specify\n"
             "                  a range (now in voxels) of locations.\n"
             "       Notes: * Boxes are cumulative; that is, if you specify more\n"
             "                  than 1 box, you'll get more than one region.\n"
             "              * If a -mask and/or -cmask option is used, then\n"
             "                  the intersection of the boxes with these masks\n"
             "                  determines which voxels are output; that is,\n"
             "                  a voxel must be inside some box AND inside the\n"
             "                  mask in order to be selected for output.\n"
             "              * If boxes select more than 1 voxel, the output lines\n"
             "                  are NOT necessarily in the order of the options on\n"
             "                  the command line.\n"
             "              * Coordinates (for -xbox, -dbox, and -nbox) are relative\n"
             "                  to the first dataset on the command line.\n"
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
             "N.B.: This program doesn't work with complex-valued datasets!\n"
            ) ;

      printf("\n" MASTER_SHORTHELP_STRING ) ;

      exit(0) ;
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

      if( strcmp(argv[narg],"-quiet") == 0 ){                /* 09 May 2003 */
        verb = 0 ; narg++ ; continue ;
      }

      if( strcmp(argv[narg]+2,"box") == 0 ){                 /* 09 May 2003 */
        float xbot,xtop , ybot,ytop , zbot,ztop , btyp ;
        int nn ;
        char code = *(argv[narg]+1) ;   /* should be 'x', 'd' , 'n', or 'i' */
        switch( code ){
          case 'x': btyp = BOX_XYZ ; break ;
          case 'd': btyp = BOX_DIC ; break ;
          case 'n': btyp = BOX_NEU ; break ;
          case 'i': btyp = BOX_IJK ; break ;
          default:
            fprintf(stderr,"** Unknown 'box' option %s\n",argv[narg]); exit(1);
        }
        if( narg+3 >= argc ){
          fprintf(stderr,"** need 3 arguments after %s\n",argv[narg]); exit(1);
        }
        nn = sscanf( argv[narg+1] , "%f:%f" , &xbot , &xtop ) ;
        if( nn < 1 ){
          fprintf(stderr,"** Can't decode %s after %s\n",argv[narg+1],argv[narg]); exit(1);
        } else if( nn == 1 ){
          xtop=xbot ;
        }
        nn = sscanf( argv[narg+2] , "%f:%f" , &ybot , &ytop ) ;
        if( nn < 1 ){
          fprintf(stderr,"** Can't decode %s after %s\n",argv[narg+2],argv[narg]); exit(1);
        } else if( nn == 1 ){
          ytop=ybot ;
        }
        nn = sscanf( argv[narg+3] , "%f:%f" , &zbot , &ztop ) ;
        if( nn < 1 ){
          fprintf(stderr,"** Can't decode %s after %s\n",argv[narg+3],argv[narg]); exit(1);
        } else if( nn == 1 ){
          ztop=zbot ;
        }
        box_dat = (float *) realloc( box_dat , sizeof(float)*BOXLEN*(box_num+1) ) ;
        box_dat[0+BOXLEN*box_num] = xbot ;
        box_dat[1+BOXLEN*box_num] = xtop ;
        box_dat[2+BOXLEN*box_num] = ybot ;
        box_dat[3+BOXLEN*box_num] = ytop ;
        box_dat[4+BOXLEN*box_num] = zbot ;
        box_dat[5+BOXLEN*box_num] = ztop ;
        box_dat[6+BOXLEN*box_num] = btyp ; box_num++ ;
        narg += 4 ; continue ;
      }

      /*-- 09 May 2003: option to output index value (for Mike B) [rickr] --*/
      if( strcmp(argv[narg],"-index") == 0 ){
	 yes_index = 1 ;
	 narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-noijk") == 0 ){
         noijk = 1 ;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-xyz") == 0 ){   /* 23 Mar 2003 */
         yes_xyz = 1 ;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-cmask") == 0 ){  /* 16 Mar 2000 */
         if( narg+1 >= argc ){
            fprintf(stderr,"** -cmask option requires a following argument!\n");
            exit(1) ;
         }
         cmask = EDT_calcmask( argv[++narg] , &ncmask ) ;
         if( cmask == NULL ){
            fprintf(stderr,"** Can't compute -cmask!\n"); exit(1);
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL ){
            fprintf(stderr,"** Cannot have two -mask options!\n") ; exit(1) ;
         }
         if( narg+1 >= argc ){
            fprintf(stderr,"** -mask option requires a following argument!\n");
            exit(1) ;
         }
         mask_dset = THD_open_dataset( argv[++narg] ) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"** Cannot open mask dataset!\n") ; exit(1) ;
         }
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex ){
            fprintf(stderr,"** Cannot deal with complex-valued mask dataset!\n");
            exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mrange",5) == 0 ){
         if( narg+2 >= argc ){
            fprintf(stderr,"** -mrange option requires 2 following arguments!\n");
             exit(1) ;
         }
         mask_bot = strtod( argv[++narg] , NULL ) ;
         mask_top = strtod( argv[++narg] , NULL ) ;
         if( mask_top < mask_top ){
            fprintf(stderr,"** -mrange inputs are illegal!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-o") == 0 ){
         if( narg+1 >= argc ){
            fprintf(stderr,"** -o needs an argument after it!\n"); exit(1);
         }
         oname = argv[++narg] ;
         if( ! THD_filename_ok(oname) ){
            fprintf(stderr,"** name after -o is illegal!\n"); exit(1) ;
         }
         if( THD_is_file(oname) ){
            fprintf(stderr,"** file %s already exists!\n",oname); exit(1);
         }
         narg++ ; continue ;
      }

      fprintf(stderr,"** Unknown option: %s\n",argv[narg]) ; exit(1) ;
   }

   /* should have at least one more argument */

   ndset = argc - narg ;
   if( ndset <= 0 ){
      fprintf(stderr,"** No input dataset!?\n") ; exit(1) ;
   }

   /* open all input datasets */

   input_dset = (THD_3dim_dataset **) malloc( sizeof(THD_3dim_dataset *)*ndset ) ;
   for( ndval=ii=0 ; ii < ndset ; ii++ ){
      input_dset[ii] = THD_open_dataset( argv[narg+ii] ) ;
      if( input_dset[ii] == NULL ){
         fprintf(stderr,"** Can't open dataset %s!\n",argv[narg+ii]) ;
         exit(1) ;
      }

      if( DSET_BRICK_TYPE(input_dset[ii],0) == MRI_complex ){
         fprintf(stderr,"** Cannot deal with complex-valued input dataset!\n");
         exit(1) ;
      }

      if( ii == 0 ){
         nvox = DSET_NVOX(input_dset[0]) ;
         nx   = DSET_NX(input_dset[0]) ;
         ny   = DSET_NY(input_dset[0]) ; nxy  = nx*ny ;
         nz   = DSET_NZ(input_dset[0]) ; nxyz = nxy*nz ;
      } else {
         if( DSET_NVOX(input_dset[ii]) != nvox ){
            fprintf(stderr,"** Dataset %s does not match in size!\n",argv[narg+ii]);
            exit(1) ;
         }
      }

      ndval += DSET_NVALS(input_dset[ii]) ;
   }

   /* make a byte mask from mask dataset */

   if( mask_dset == NULL ){
      mmm = NULL ;
      if( verb ) fprintf(stderr,"++ %d voxels in the entire dataset (no mask)\n",nvox) ;
   } else {
      if( DSET_NVOX(mask_dset) != nvox ){
         fprintf(stderr,"** Input and mask datasets are not same dimensions!\n");
         exit(1) ;
      }
      mmm = THD_makemask( mask_dset , 0 , mask_bot,mask_top ) ;
      mcount = THD_countmask( nvox , mmm ) ;
      if( mcount <= 0 ){
         fprintf(stderr,"** No voxels in the mask!\n") ; exit(1) ;
      }
      if( verb ) fprintf(stderr,"++ %d voxels in the mask\n",mcount) ;
      DSET_delete(mask_dset) ;
   }

   /* 16 Mar 2000: deal with the cmask */

   if( cmask != NULL ){
      if( ncmask != nvox ){
         fprintf(stderr,"** Input and cmask datasets are not same dimensions!\n");
         exit(1) ;
      }
      if( mmm != NULL ){
         for( ii=0 ; ii < nvox ; ii++ )
            mmm[ii] = (mmm[ii] && cmask[ii]) ;
         free(cmask) ;
         mcount = THD_countmask( nvox , mmm ) ;
         if( mcount <= 0 ){
            fprintf(stderr,"** No voxels in the mask+cmask!\n") ; exit(1) ;
         }
         if( verb ) fprintf(stderr,"++ %d voxels in the mask+cmask\n",mcount) ;
      } else {
         mmm = cmask ;
         mcount = THD_countmask( nvox , mmm ) ;
         if( mcount <= 0 ){
            fprintf(stderr,"** No voxels in the cmask!\n") ; exit(1) ;
         }
         if( verb ) fprintf(stderr,"++ %d voxels in the cmask\n",mcount) ;
      }
   }

   /* 09 May 2003: make a mask corresponding to the boxen */

   if( box_num > 0 ){
     byte *bmask = calloc(1,nvox) ;
     int bb, ibot,itop, jbot,jtop, kbot,ktop , btyp , ii,jj,kk ;
     float   xbot,xtop, ybot,ytop, zbot,ztop ;
     THD_fvec3 dv,xv ;
     THD_3dim_dataset *dset = input_dset[0] ;
     float xmin=dset->daxes->xxmin , xmax=dset->daxes->xxmax ;
     float ymin=dset->daxes->yymin , ymax=dset->daxes->yymax ;
     float zmin=dset->daxes->zzmin , zmax=dset->daxes->zzmax ;

     for( bb=0 ; bb < box_num ; bb++ ){
       xbot = box_dat[0+BOXLEN*bb]; xtop = box_dat[1+BOXLEN*bb];
       ybot = box_dat[2+BOXLEN*bb]; ytop = box_dat[3+BOXLEN*bb];
       zbot = box_dat[4+BOXLEN*bb]; ztop = box_dat[5+BOXLEN*bb];
       btyp = box_dat[6+BOXLEN*bb];

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
       if ( itop < 0 || ibot >= nx-1 ) continue;
       if ( jtop < 0 || jbot >= ny-1 ) continue;
       if ( ktop < 0 || kbot >= nz-1 ) continue;

       /* constrain values to dataset dimensions */
       if ( ibot < 0 ) ibot = 0;  if ( itop >= nx-1 ) itop = nx-1;
       if ( jbot < 0 ) jbot = 0;  if ( jtop >= ny-1 ) jtop = ny-1;
       if ( kbot < 0 ) kbot = 0;  if ( ktop >= nz-1 ) ktop = nz-1;

       for( kk=kbot ; kk <= ktop ; kk++ )
        for( jj=jbot ; jj <= jtop ; jj++ )
         for( ii=ibot ; ii <= itop ; ii++ )
           bmask[ii+jj*nx+kk*nxy] = 1 ;
     }

     mcount = THD_countmask( nvox , bmask ) ;
     if( verb ) fprintf(stderr,"++ %d voxels in the boxes\n",mcount) ;
     if( mcount == 0 ){
       fprintf(stderr,"** Can't continue with no voxels insides boxes!\n"); exit(1);
     }
     if( mmm != NULL ){
       for( ii=0 ; ii < nvox ; ii++ )
          mmm[ii] = (mmm[ii] && bmask[ii]) ;
       free(bmask) ;
       mcount = THD_countmask( nvox , mmm ) ;
       if( mcount <= 0 ){
          fprintf(stderr,"** No voxels in the mask+boxes!\n") ; exit(1) ;
       }
       if( verb ) fprintf(stderr,"++ %d voxels in the mask+boxes\n",mcount) ;
     } else {
       mmm = bmask ;
     }
   } /* end of box mask */

   /* read in input dataset bricks */

   for( ii=0 ; ii < ndset ; ii++ ){
      DSET_load(input_dset[ii]) ;
      if( !DSET_LOADED(input_dset[ii]) ){
         fprintf(stderr,"*** Can't load dataset %s\n",argv[narg+ii]); exit(1);
      }
   }

   /* open the output file */

   if( oname != NULL ){
      ofile = fopen( oname , "w" ) ;
      if( ofile == NULL ){
         fprintf(stderr,"*** Can't open output file %s\n",oname); exit(1);
      }
   } else {
      ofile = stdout ;
   }

   /* output string buffers */

   /*-- 09 May 2003: add room for the index (9 -> 10)    [rickr] --*/
   obuf = (char *) malloc( sizeof(char) * (ndval+10) * 16 ) ;

   /* loop over voxels */

   for( ii=0 ; ii < nvox ; ii++ ){
      if( mmm != NULL && mmm[ii] == 0 ) continue ;  /* skip this voxel */

      obuf[0] = '\0' ;

      /*-- 09 May 2003: optionally display index    [rickr] --*/
      if ( yes_index ){
         otemp = MV_format_fval((float)ii);
	 strcat(obuf,otemp); strcat(obuf," ");
      }

      if( ! noijk ){
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

      for( kk=0 ; kk < ndset ; kk++ ){
         flim = THD_extract_series( ii , input_dset[kk] , 0 ) ;
         flar = MRI_FLOAT_PTR(flim) ;
         for( vv=0 ; vv < flim->nx ; vv++ ){
            otemp = MV_format_fval(flar[vv]) ;
            strcat(obuf,otemp) ; strcat(obuf," ") ;
         }
         mri_free(flim) ;
      }

      jj = strlen(obuf) ; obuf[jj-1] = '\0' ; /* kill last blank */

      fprintf(ofile,"%s\n",obuf) ;
   }

   fflush(ofile) ; fsync(fileno(ofile)) ; fclose(ofile) ;
   exit(0) ;
}

