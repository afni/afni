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
   THD_3dim_dataset * mask_dset=NULL , ** input_dset=NULL ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   byte * mmm   = NULL ;
   char * oname = NULL , * obuf , * otemp ;
   FILE * ofile ;
   MRI_IMAGE * flim ;
   float * flar ;
   int noijk=0 ;
   byte * cmask=NULL ; int ncmask=0 ;

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
             "  -noijk       Means not to write out the i,j,k values.\n"
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

      if( strcmp(argv[narg],"-noijk") == 0 ){
         noijk = 1 ;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-cmask") == 0 ){  /* 16 Mar 2000 */
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -cmask option requires a following argument!\n");
            exit(1) ;
         }
         cmask = EDT_calcmask( argv[++narg] , &ncmask ) ;
         if( cmask == NULL ){
            fprintf(stderr,"*** Can't compute -cmask!\n"); exit(1);
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL ){
            fprintf(stderr,"*** Cannot have two -mask options!\n") ; exit(1) ;
         }
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -mask option requires a following argument!\n");
            exit(1) ;
         }
         mask_dset = THD_open_dataset( argv[++narg] ) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"*** Cannot open mask dataset!\n") ; exit(1) ;
         }
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex ){
            fprintf(stderr,"*** Cannot deal with complex-valued mask dataset!\n");
            exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mrange",5) == 0 ){
         if( narg+2 >= argc ){
            fprintf(stderr,"*** -mrange option requires 2 following arguments!\n");
             exit(1) ;
         }
         mask_bot = strtod( argv[++narg] , NULL ) ;
         mask_top = strtod( argv[++narg] , NULL ) ;
         if( mask_top < mask_top ){
            fprintf(stderr,"*** -mrange inputs are illegal!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-o") == 0 ){
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -o needs an argument after it!\n"); exit(1);
         }
         oname = argv[++narg] ;
         if( ! THD_filename_ok(oname) ){
            fprintf(stderr,"*** name after -o is illegal!\n"); exit(1) ;
         }
         if( THD_is_file(oname) ){
            fprintf(stderr,"*** file %s already exists!\n",oname); exit(1);
         }
         narg++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[narg]) ; exit(1) ;
   }

   /* should have at least one more argument */

   ndset = argc - narg ;
   if( ndset <= 0 ){
      fprintf(stderr,"*** No input dataset!?\n") ; exit(1) ;
   }

   /* open all input datasets */

   input_dset = (THD_3dim_dataset **) malloc( sizeof(THD_3dim_dataset *)*ndset ) ;
   for( ndval=ii=0 ; ii < ndset ; ii++ ){
      input_dset[ii] = THD_open_dataset( argv[narg+ii] ) ;
      if( input_dset[ii] == NULL ){
         fprintf(stderr,"*** Can't open dataset %s!\n",argv[narg+ii]) ;
         exit(1) ;
      }

      if( DSET_BRICK_TYPE(input_dset[ii],0) == MRI_complex ){
         fprintf(stderr,"*** Cannot deal with complex-valued input dataset!\n");
         exit(1) ;
      }

      if( ii == 0 ){
         nvox = DSET_NVOX(input_dset[0]) ;
      } else {
         if( DSET_NVOX(input_dset[ii]) != nvox ){
            fprintf(stderr,"*** Dataset %s does not match in size!\n",argv[narg+ii]);
            exit(1) ;
         }
      }

      ndval += DSET_NVALS(input_dset[ii]) ;
   }

   /* make a byte mask from mask dataset */

   if( mask_dset == NULL ){
      mmm = NULL ;
      fprintf(stderr,"+++ %d voxels in the entire dataset (no mask)\n",nvox) ;
   } else {
      if( DSET_NVOX(mask_dset) != nvox ){
         fprintf(stderr,"*** Input and mask datasets are not same dimensions!\n");
         exit(1) ;
      }
      mmm = THD_makemask( mask_dset , 0 , mask_bot,mask_top ) ;
      mcount = THD_countmask( nvox , mmm ) ;
      if( mcount <= 0 ){
         fprintf(stderr,"*** No voxels in the mask!\n") ; exit(1) ;
      }
      fprintf(stderr,"+++ %d voxels in the mask\n",mcount) ;
      DSET_delete(mask_dset) ;
   }

   /* 16 Mar 2000: deal with the cmask */

   if( cmask != NULL ){
      if( ncmask != nvox ){
         fprintf(stderr,"*** Input and cmask datasets are not same dimensions!\n");
         exit(1) ;
      }
      if( mmm != NULL ){
         for( ii=0 ; ii < nvox ; ii++ )
            mmm[ii] = (mmm[ii] && cmask[ii]) ;
         free(cmask) ;
         mcount = THD_countmask( nvox , mmm ) ;
         if( mcount <= 0 ){
            fprintf(stderr,"*** No voxels in the mask+cmask!\n") ; exit(1) ;
         }
         fprintf(stderr,"+++ %d voxels in the mask+cmask\n",mcount) ;
      } else {
         mmm = cmask ;
         mcount = THD_countmask( nvox , mmm ) ;
         if( mcount <= 0 ){
            fprintf(stderr,"*** No voxels in the cmask!\n") ; exit(1) ;
         }
         fprintf(stderr,"+++ %d voxels in the cmask\n",mcount) ;
      }
   }

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

   obuf = (char *) malloc( sizeof(char) * (ndval+3) * 16 ) ;

   /* loop over voxels */

   for( ii=0 ; ii < nvox ; ii++ ){
      if( mmm != NULL && mmm[ii] == 0 ) continue ;  /* skip this voxel */

      obuf[0] = '\0' ;

      if( ! noijk ){
         i = DSET_index_to_ix( input_dset[0] , ii ) ;  /* voxel indexes */
         j = DSET_index_to_jy( input_dset[0] , ii ) ;
         k = DSET_index_to_kz( input_dset[0] , ii ) ;

         otemp = MV_format_fval((float)i); strcat(obuf,otemp); strcat(obuf," ");
         otemp = MV_format_fval((float)j); strcat(obuf,otemp); strcat(obuf," ");
         otemp = MV_format_fval((float)k); strcat(obuf,otemp); strcat(obuf," ");
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
