/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*------------------------------------------------------------------------*/

static float min_float( int n , float *x )   /* 25 Feb 2005 */
{
   float m=0.0 ; int i ;
   if( n < 1 || x == NULL ) return m ;
   m = x[0] ;
   for( i=1 ; i < n ; i++ ) if( m > x[i] ) m = x[i] ;
   return m ;
}

/*------------------------------------------------------------------------*/

static float max_float( int n , float *x )   /* 24 Feb 2005 */
{
   float m=0.0 ; int i ;
   if( n < 1 || x == NULL ) return m ;
   m = x[0] ;
   for( i=1 ; i < n ; i++ ) if( m < x[i] ) m = x[i] ;
   return m ;
}
void usage_3dmaskave(int detail) {
        printf(
"Usage: 3dmaskave [options] inputdataset\n"
"\n"
"Computes average of all voxels in the input dataset\n"
"which satisfy the criterion in the options list.\n"
"If no options are given, then all voxels are included.\n"
"\n"                         /* examples: 13 Mar 2006 [rickr] */
"----------------------------------------------------------------\n"
"Examples:\n"
"\n"
"1. compute the average timeseries in epi_r1+orig, over voxels\n"
"   that are set (any non-zero value) in the dataset, ROI+orig:\n"
"\n"
"    3dmaskave -mask ROI+orig epi_r1+orig\n"
"\n"
"2. restrict the ROI to values of 3 or 4, and save (redirect)\n"
"   the output to the text file run1_roi_34.txt:\n"
"\n"
"    3dmaskave -mask ROI+orig -quiet -mrange 3 4   \\\n"
"              epi_r1+orig > run1_roi_34.txt\n"
"\n"
"3. Extract the time series from a single voxel with given\n"
"   spatial indexes (e.g., for use with 3dTcorr1D):\n"
"\n"
"    3dmaskave -quiet -ibox 40 30 20 epi_r1+orig > r1_40_30_20.1D\n"
"----------------------------------------------------------------\n"
"\n"
"Options:\n"
"  -mask mset   Means to use the dataset 'mset' as a mask:\n"
"                 Only voxels with nonzero values in 'mset'\n"
"                 will be averaged from 'dataset'.  Note\n"
"                 that the mask dataset and the input dataset\n"
"                 must have the same number of voxels.\n"
"               SPECIAL CASE: If 'mset' is the string 'SELF',\n"
"                             then the input dataset will be\n"
"                             used to mask itself.  That is,\n"
"                             only nonzero voxels from the\n"
"                             #miv sub-brick will be used.\n"
"\n"
"  -mindex miv  Means to use sub-brick #'miv' from the mask\n"
"                 dataset.  If not given, miv=0.\n"
"  -mrange a b  Means to further restrict the voxels from\n"
"                 'mset' so that only those mask values\n"
"                 between 'a' and 'b' (inclusive) will\n"
"                 be used.  If this option is not given,\n"
"                 all nonzero values from 'mset' are used.\n"
"                 Note that if a voxel is zero in 'mset', then\n"
"                 it won't be included, even if a < 0 < b.\n"
"                   [-mindex and -mrange are old options that predate]\n"
"                   [the introduction of the sub-brick selector '[]' ]\n"
"                   [and the sub-range value selector '<>' to AFNI.  ]\n"
"\n"
"  -xbox x y z     } These options are the same as in\n"
"  -dbox x y z     } program 3dmaskdump:\n"
"  -nbox x y z     } They create a mask by putting down boxes\n"
"  -ibox x y z     } or balls (filled spheres) at the specified\n"
"  -xball x y z r  } locations.  See the output of\n"
"  -dball x y z r  }   3dmaskdump -help\n"
"  -nball x y z r  } for the gruesome and tedious details.\n"
"   https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dmaskdump.html\n"
"\n"
"  -dindex div  Means to use sub-brick #'div' from the inputdataset.\n"
"                 If not given, all sub-bricks will be processed.\n"
"  -drange a b  Means to only include voxels from the inputdataset whose\n"
"                 values fall in the range 'a' to 'b' (inclusive).\n"
"                 Otherwise, all voxel values are included.\n"
"                   [-dindex and -drange are old options that predate]\n"
"                   [the introduction of the sub-brick selector '[]' ]\n"
"                   [and the sub-range value selector '<>' to AFNI.  ]\n"
"\n"
"  -slices p q  Means to only included voxels from the inputdataset\n"
"                 whose slice numbers are in the range 'p' to 'q'\n"
"                 (inclusive).  Slice numbers range from 0 to\n"
"                 NZ-1, where NZ can be determined from the output\n"
"                 of program 3dinfo.  The default is to include\n"
"                 data from all slices.\n"
"                   [There is no provision for geometrical voxel]\n"
"                   [selection except in the slice (z) direction]\n"
"\n"
"  -sigma       Means to compute the standard deviation in addition\n"
"                 to the mean.\n"
"  -sum         Means to compute the sum instead of the mean.\n"
"  -sumsq       Means to compute the sum of squares instead of the mean.\n"
"  -enorm       Means to compute the Euclidean norm instead of the mean.\n"
"               This is the sqrt() of the sumsq result.\n"
"  -median      Means to compute the median instead of the mean.\n"
"  -max         Means to compute the max instead of the mean.\n"
"  -min         Means to compute the min instead of the mean.\n"
"                 [-sigma is ignored with -sum, -median, -max, or -min.]\n"
"                 [the last given of -sum, -median, -max, or -min wins.]\n"
"  -perc XX     Means to compute the XX-th percentile value (min=0 max=100).\n"
"               XX should be an integer from 0 to 100.\n"
"  -dump        Means to print out all the voxel values that\n"
"                 go into the result.\n"
"  -udump       Means to print out all the voxel values that\n"
"                 go into the average, UNSCALED by any internal\n"
"                 factors.\n"
"                 N.B.: the scale factors for a sub-brick\n"
"                       can be found using program 3dinfo.\n"
"  -indump      Means to print out the voxel indexes (i,j,k) for\n"
"                 each dumped voxel.  Has no effect if -dump\n"
"                 or -udump is not also used.\n"
"                 N.B.: if nx,ny,nz are the number of voxels in\n"
"                       each direction, then the array offset\n"
"                       in the brick corresponding to (i,j,k)\n"
"                       is i+j*nx+k*nx*ny.\n"
" -q     or\n"
" -quiet        Means to print only the minimal numerical result(s).\n"
"               This is useful if you want to create a *.1D file,\n"
"               without any extra text; for example:\n"
"                 533.814 [18908 voxels]   ==   'normal' output\n"
"                 533.814                  ==   'quiet' output\n"
"\n"
"The output is printed to stdout (the terminal), and can be\n"
"saved to a file using the usual redirection operation '>'.\n"
"\n"
"Or you can do fun stuff like\n"
"  3dmaskave -q -mask Mfile+orig timefile+orig | 1dplot -stdin -nopush\n"
"to pipe the output of 3dmaskave into 1dplot for graphing.\n"
"\n"
"-- Author: RWCox\n"
            ) ;

      printf("\n" MASTER_SHORTHELP_STRING ) ;

      PRINT_COMPILE_DATE ; 
   
   return;
}

/*-----------------------------------------------------------------------
  A quickie.  (Not so quick any more, with all the options added.)
-------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int narg , nvox , ii , mcount , iv , mc ;
   THD_3dim_dataset * mask_dset=NULL , * input_dset=NULL ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   byte * mmm = NULL ;
   int dumpit = 0 , sigmait = 0 ;
   int miv = 0 ;                              /* 06 Aug 1998 */
   int div = -1 , div_bot,div_top , drange=0; /* 16 Sep 1998 */
   float data_bot=666.0 , data_top=-666.0 ;
   int indump = 0 ;                           /* 19 Aug 1999 */
   int pslice=-1 , qslice=-1 , nxy=0,nz=0 ;       /* 15 Sep 1999 */
   int quiet=0 ;                              /* 23 Nov 1999 */

   int medianit = 0 ;                         /* 06 Jul 2003 */
   int maxit    = 0 ;                         /* 24 Feb 2005 */
   int minit    = 0 ;                         /* 25 Feb 2005 */
   int sumit    = 0 ;                         /* 15 Jun 2011 */
   int sumsqit  = 0 , enormit  = 0 ;          /* 16 Apr 2013 */
   int percit   = 0 ; float perc = 0.0f ;     /* 21 Jun 2016 */
   float *exar ;                              /* 06 Jul 2003 */
   char *sname = "Average" ;                  /* 06 Jul 2003 */
   int self_mask = 0 ;                        /* 06 Dec 2004 */

   int    boxball_num=0 ;                     /* 09 Sep 2009 */
   float *boxball_dat=NULL ;

   mainENTRY("3dmaskave main"); machdep(); AFNI_logger("3dmaskave",argc,argv);
   PRINT_VERSION("3dmaskave") ;

   /* scan argument list */
   if (argc == 1) { usage_3dmaskave(1); exit(0); } /* Bob's help shortcut */
   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){
      if (strcmp(argv[narg], "-h") == 0 || strcmp(argv[narg], "-help") == 0) {
         usage_3dmaskave(strlen(argv[narg]) > 3 ? 2:1);
         exit(0);
      }

      if( strcmp(argv[narg]+2,"box")  == 0 ||    /* 09 Sep 2009 */
          strcmp(argv[narg]+2,"ball") == 0   ){

        int nuse = THD_parse_boxball( &boxball_num , &boxball_dat , argv+narg ) ;
        if( nuse <= 0 ) ERROR_exit("Can't interpret '%s' values",argv[narg]) ;
        narg += nuse ; continue ;
      }

      if( strcmp(argv[narg],"-q") == 0 || strcmp(argv[narg],"-quiet") == 0 ){
         quiet++ ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL || self_mask ){
           fprintf(stderr,"*** Cannot have two -mask options!\n") ; exit(1) ;
         }
         if( narg+1 >= argc ){
           fprintf(stderr,"*** -mask option requires a following argument!\n") ;
           exit(1) ;
         }
         narg++ ;
         if( strcmp(argv[narg],"SELF") == 0 ){
           self_mask = 1 ;
         } else {
           mask_dset = THD_open_dataset( argv[narg] ) ;
           if( mask_dset == NULL ){
             fprintf(stderr,"*** Cannot open mask dataset!\n") ; exit(1) ;
           }
           if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex ){
             fprintf(stderr,"*** Cannot deal with complex-valued mask dataset!\n") ;
             exit(1) ;
           } else if( DSET_BRICK_TYPE(mask_dset,0) == MRI_rgb ){
             fprintf(stderr,"*** Cannot deal with rgb-valued mask dataset!\n") ;
             exit(1) ;
           }
         }
         narg++ ; continue ;
      }

      /* 06 Aug 1998 */

      if( strncmp(argv[narg],"-mindex",5) == 0 ){
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -mindex option needs 1 following argument!\n") ; exit(1) ;
         }
         miv = (int) strtod( argv[++narg] , NULL ) ;
         if( miv < 0 ){
            fprintf(stderr,"*** -mindex value is negative!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-mrange",5) == 0 ){
         if( narg+2 >= argc ){
            fprintf(stderr,"*** -mrange option requires 2 following arguments!\n") ; exit(1) ;
         }
         mask_bot = strtod( argv[++narg] , NULL ) ;
         mask_top = strtod( argv[++narg] , NULL ) ;
         if( mask_top < mask_top ){
            fprintf(stderr,"*** -mrange inputs are illegal!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      /* 16 Sep 1998 */

      if( strncmp(argv[narg],"-dindex",5) == 0 ){
         if( narg+1 >= argc ){
            fprintf(stderr,"*** -dindex option needs 1 following argument!\n") ; exit(1) ;
         }
         div = (int) strtod( argv[++narg] , NULL ) ;
         if( div < 0 ){
            fprintf(stderr,"*** -dindex value is negative!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-drange",5) == 0 ){
         if( narg+2 >= argc ){
            fprintf(stderr,
                    "*** -drange option requires 2 following arguments!\n") ;
            exit(1) ;
         }
         data_bot = strtod( argv[++narg] , NULL ) ;
         data_top = strtod( argv[++narg] , NULL ) ;
         if( data_top < data_top ){
            fprintf(stderr,"*** -mrange inputs are illegal!\n") ; exit(1) ;
         }
         drange = 1 ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-slices",5) == 0 ){  /* 15 Sep 1999 */
         if( narg+2 >= argc ){
            fprintf(stderr,
                    "*** -slices option requires 2 following arguments!\n") ;
            exit(1) ;
         }
         pslice = (int) strtod( argv[++narg] , NULL ) ;
         qslice = (int) strtod( argv[++narg] , NULL ) ;
         if( pslice < 0 || qslice < 0 || qslice < pslice ){
            fprintf(stderr, "*** Illegal values after -slices!\n") ;
            exit(1) ;
         }
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-dump",5) == 0 ){
         dumpit = 1 ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-udump",5) == 0 ){
         dumpit = 2 ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-indump",5) == 0 ){  /* 19 Aug 1999 */
         indump = 1 ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-sigma",5) == 0 ){
         sigmait = 2 ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-median",5) == 0 ){
         medianit = 1 ; maxit = 0 ; minit = 0 ; sumit = 0 ;
                        sumsqit = 0 ; enormit = 0 ; percit = 0 ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-perc",5) == 0 ){
        if( narg+1 > argc ) ERROR_exit("need argument after '-perc'") ;
        perc = (float)strtod(argv[++narg],NULL) ;
        if( perc < 0.0f || perc > 100.0f )
          ERROR_exit("'-perc %s' is not legal [range is 0..100]",argv[narg]) ;
        if( perc == 0.0f ){
          WARNING_message("'-perc 0' is the same as '-min'") ;
          maxit = 0 ; medianit = 0 ; minit = 1 ; sumit = 0 ;
                      sumsqit = 0 ; enormit = 0 ; percit = 0 ;
        } else if( perc == 100.0f ){
          WARNING_message("'-perc 100' is the same as '-max'") ;
          maxit = 1 ; medianit = 0 ; minit = 0 ; sumit = 0 ;
                      sumsqit = 0 ; enormit = 0 ; percit = 0 ;
        } else if( perc == 50.0f ){
          WARNING_message("'-perc 50' is the same as '-median'") ;
          maxit = 0 ; medianit = 1 ; minit = 0 ; sumit = 0 ;
                      sumsqit = 0 ; enormit = 0 ; percit = 0 ;
        } else {
          maxit = 0 ; medianit = 0 ; minit = 0 ; sumit = 0 ;
                      sumsqit = 0 ; enormit = 0 ;
          percit = 1 ;
        }
        narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-max",4) == 0 ){  /* 24 Feb 2005 */
         maxit = 1 ; medianit = 0 ; minit = 0 ; sumit = 0 ;
                     sumsqit = 0 ; enormit = 0 ; percit = 0 ;
         narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-sum") == 0 ){  /* 15 Jun 2011, P.S. no strncmp */
        sumit = 1 ; maxit = medianit = minit = 0 ; percit = 0 ;
        narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-sumsq") == 0 ){  /* 16 Apr 2013 [rickr] */
        sumsqit = 1 ; sumit = maxit = medianit = minit = 0 ; percit = 0 ;
        narg++ ; continue ;
      }

      if( strcmp(argv[narg],"-enorm") == 0 ){  /* 16 Apr 2013 [rickr] */
        /* enorm implies sumsq, since enorm just takes sqrt */
        enormit = sumsqit = 1 ; sumit = maxit = medianit = minit = 0 ; percit = 0 ;
        narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-min",4) == 0 ){  /* 25 Feb 2005 */
         maxit = 0 ; medianit = 0 ; minit = 1 ; sumit = 0 ;
                     sumsqit = 0 ; enormit = 0 ; percit = 0 ;
         narg++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[narg]) ; 
      suggest_best_prog_option(argv[0], argv[narg]);
      exit(1) ;
   }
   
   if (argc < 2) {
      ERROR_message("Too few options, use -help option for details");
      exit(1);
   }
   
   if( medianit ){ sigmait = 0; sname = "Median"; }
   if( maxit    ){ sigmait = 0; sname = "Max"   ; } /* 24 Feb 2005 */
   if( minit    ){ sigmait = 0; sname = "Min"   ; } /* 25 Feb 2005 */
   if( sumit    ){ sigmait = 0; sname = "Sum"   ; } /* 15 Jun 2011 */
   if( sumsqit  ){ sigmait = 0; sname = "Sumsq" ; } /* 16 Apr 2013 */
   if( enormit  ){ sigmait = 0; sname = "Enorm" ; } /* 16 Apr 2013 */
   if( percit   ){ sigmait = 0;
                   sname   = (char *)malloc(sizeof(char)*16) ;
                   sprintf(sname,"Perc%02d",(int)rintf(perc)) ; }

   /* should have one more argument */

   if( narg >= argc ){
      fprintf(stderr,"*** No input dataset!?\n") ; exit(1) ;
   }

#if 0
   if( dumpit && mask_dset == NULL ){
     fprintf(stderr,"*** Can't use dump option without -mask!\n") ; exit(1) ;
   }
#endif

   /* read input dataset */

   input_dset = THD_open_dataset( argv[narg] ) ;
   if( input_dset == NULL ){
     fprintf(stderr,"*** Cannot open input dataset %s!\n",argv[narg]) ; exit(1) ;
   }
   if( self_mask ) mask_dset = input_dset ;  /* 06 Dec 2004 */

   if( miv > 0 ){                /* 06 Aug 1998 */
     if( mask_dset == NULL ){
       fprintf(stderr,"*** -mindex option used without -mask!\n") ; exit(1) ;
     }
     if( miv >= DSET_NVALS(mask_dset) ){
       fprintf(stderr,"*** -mindex value is too large!\n") ; exit(1) ;
     }
   }

   if( DSET_BRICK_TYPE(input_dset,0) == MRI_complex ){
     fprintf(stderr,"*** Cannot deal with complex-valued input dataset!\n") ; exit(1) ;
   }

   if( div >= DSET_NVALS(input_dset) ){
     fprintf(stderr,"*** Not enough sub-bricks in dataset for -dindex %d!\n",div) ;
     exit(1) ;
   }

   if( pslice >= 0 ){
     nxy = DSET_NX(input_dset) * DSET_NY(input_dset) ;
     nz  = DSET_NZ(input_dset) ;
     if( qslice >= nz ){
       fprintf(stderr,
               "*** There are only %d slices in the input dataset!\n",nz) ;
       exit(1) ;
     }

     if( pslice == 0 && qslice == nz-1 )
       fprintf(stderr,"+++ -slice option says to use all slices!?\n") ;
   }

   nvox = DSET_NVOX(input_dset) ;

   /* make a byte mask from mask dataset */

   mmm = (byte *) malloc( sizeof(byte) * nvox ) ;
   if( mmm == NULL ){
      fprintf(stderr,"*** Cannot malloc workspace!\n") ; exit(1) ;
   }

   if( mask_dset != NULL ){
      if( !EQUIV_GRIDS(mask_dset,input_dset) )
         WARNING_message("Input dataset %s grid mismatch from mask.\n"
             "Try the following command for grid comparison:\n"
             " 3dinfo -header_line -prefix -same_all_grid %s %s\n"
             ,argv[narg], 
             DSET_HEADNAME(mask_dset), DSET_HEADNAME(input_dset)) ;

      if( DSET_NVOX(mask_dset) != nvox ){
        fprintf(stderr,"*** Input and mask datasets are not same dimensions!\n") ;
        exit(1) ;
      }
      DSET_load(mask_dset) ;
      if( DSET_ARRAY(mask_dset,miv) == NULL ){
        fprintf(stderr,"*** Cannot read in mask dataset BRIK!\n") ; exit(1) ;
      }

      switch( DSET_BRICK_TYPE(mask_dset,miv) ){
        default:
          fprintf(stderr,"*** Cannot deal with mask dataset datum!\n") ; exit(1) ;

        case MRI_short:{
          short mbot , mtop ;
          short * mar = (short *) DSET_ARRAY(mask_dset,miv) ;
          float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
          if( mfac == 0.0 ) mfac = 1.0 ;
          if( mask_bot <= mask_top ){
            mbot = SHORTIZE(mask_bot/mfac) ;
            mtop = SHORTIZE(mask_top/mfac) ;
          } else {
            mbot = (short) -MRI_TYPE_maxval[MRI_short] ;
            mtop = (short)  MRI_TYPE_maxval[MRI_short] ;
          }
          for( mcount=0,ii=0 ; ii < nvox ; ii++ )
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii] = 1 ; mcount++ ; }
            else                                                    { mmm[ii] = 0 ; }
        }
        break ;

        case MRI_byte:{
          byte mbot , mtop ;
          byte * mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
          float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
          if( mfac == 0.0 ) mfac = 1.0 ;
          if( mask_bot <= mask_top ){
            mbot = BYTEIZE(mask_bot/mfac) ;
            mtop = BYTEIZE(mask_top/mfac) ;
            if( mtop == 0 ){
              fprintf(stderr,"*** Illegal mask range for mask dataset of bytes.\n") ; exit(1) ;
            }
          } else {
            mbot = 0 ;
            mtop = (byte) MRI_TYPE_maxval[MRI_short] ;
          }
          for( mcount=0,ii=0 ; ii < nvox ; ii++ )
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii] = 1 ; mcount++ ; }
            else                                                    { mmm[ii] = 0 ; }
        }
        break ;

        case MRI_float:{
          float mbot , mtop ;
          float * mar = (float *) DSET_ARRAY(mask_dset,miv) ;
          float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
          if( mfac == 0.0 ) mfac = 1.0 ;
          if( mask_bot <= mask_top ){
            mbot = (float) (mask_bot/mfac) ;
            mtop = (float) (mask_top/mfac) ;
          } else {
            mbot = -WAY_BIG ;
            mtop =  WAY_BIG ;
          }
          for( mcount=0,ii=0 ; ii < nvox ; ii++ )
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii] = 1 ; mcount++ ; }
            else                                                    { mmm[ii] = 0 ; }
        }
        break ;
      }
      if( !self_mask ) DSET_unload(mask_dset) ;

      if( mcount == 0 ){
        fprintf(stderr,"*** No voxels survive the masking operation\n"); exit(1);
      }

      fprintf(stderr,"+++ %d voxels survive the mask\n",mcount) ;

   } else {
      mcount = nvox ;
      memset( mmm , 1 , mcount ) ;
      fprintf(stderr,"+++ %d voxels in the entire dataset (no mask)\n",mcount) ;
   }

   if( pslice >= 0 ){     /* 15 Sep 1999 */
      int kz , ibot ;
      mcount = 0 ;
      for( kz=0 ; kz < nz ; kz++ ){           /* loop over all slices */
         ibot = kz*nxy ;                      /* base index for this slice */

         if( kz >= pslice && kz <= qslice ){  /* keepers => recount */
            for( ii=0 ; ii < nxy ; ii++ )
               if( mmm[ii+ibot] ) mcount++ ;
         } else {                             /* throw them back */
            for( ii=0 ; ii < nxy ; ii++ )
               mmm[ii+ibot] = 0 ;
         }
      }

      if( mcount == 0 ){
         fprintf(stderr,"*** No voxels survive the slicing operation\n"); exit(1);
      }

      fprintf(stderr,"+++ %d voxels survive the slicing\n",mcount) ;
   }

   /*----- 09 Sep 2009: ball and box masks? -----*/

   if( boxball_num > 0 ){
     byte *bmask = THD_boxballmask( input_dset , boxball_num , boxball_dat ) ;
     if( bmask != NULL ){
       int bm = THD_countmask( nvox , bmask ) ;
       INFO_message("%d voxels in the boxes+balls",bm) ;
       if( bm == 0 ) ERROR_exit("Can't continue!!") ;
       for( ii=0 ; ii < nvox ; ii++ )
         mmm[ii] = (mmm[ii] && bmask[ii]) ;  /* intersection */
       free(bmask) ;
       mcount = THD_countmask( nvox , mmm ) ;
       if( mcount <= 0 ) ERROR_exit("No voxels in the mask INTERSECT boxes+balls!") ;
       INFO_message("%d voxels in the mask INTERSECT boxes+balls",mcount) ;
     }
   }

   /*-- check errors --*/

   if( mcount < 2 && sigmait ){
     WARNING_message("too few voxels; cannot compute sigma") ;
     sigmait = 0 ;
   }

   DSET_load(input_dset) ;
   if( DSET_ARRAY(input_dset,0) == NULL )
      ERROR_exit("Cannot read in input dataset BRIK!\n") ;

   /*-- loop over input sub-bricks --*/

   if( div < 0 ){
      div_bot = 0 ; div_top = DSET_NVALS(input_dset) ;
   } else {
      div_bot = div ; div_top = div+1 ;
   }

   exar = (float *) malloc( sizeof(float) * mcount ) ; /* 06 Jul 2003 */

   for( iv=div_bot ; iv < div_top ; iv++ ){

      switch( DSET_BRICK_TYPE(input_dset,iv) ){

         default:
            printf("*** Illegal sub-brick datum at %d\n",iv) ;
         break ;

#define INRANGE(i) ( !drange || ( mfac*bar[i] >= data_bot && mfac*bar[i] <= data_top ) )
#define GOOD(i)    ( mmm[i] && INRANGE(i) )

         case MRI_short:{
            short * bar = (short *) DSET_ARRAY(input_dset,iv) ;
            double sum = 0.0 , sigma = 0.0 ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 || dumpit == 2 ) mfac = 1.0 ;

            if( dumpit ){
               int noscal = (dumpit==2) || (mfac==1.0) ;
               printf("+++ Dump for sub-brick %d:\n",iv) ;
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ){
                                                 if( noscal ) printf(" %d",bar[ii]) ;
                                                 else         printf(" %g",bar[ii]*mfac) ;

                                                 if( indump )
                                                    printf(" (%d,%d,%d)",
                                                           DSET_index_to_ix(input_dset,ii),
                                                           DSET_index_to_jy(input_dset,ii),
                                                           DSET_index_to_kz(input_dset,ii) ) ;
                                                 printf("\n") ;
                                              }
            }

            for( ii=mc=0 ; ii < nvox ; ii++ )
               if( GOOD(ii) ){
                  if( sumsqit ) sum += bar[ii]*bar[ii] ;
                  else          sum += bar[ii] ;
                  exar[mc++] = bar[ii] ;
               }
            if( mc > 0 ) sum = sum / mc ;

            if( sigmait && mc > 1 ){
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mc-1) ) ;
            }

                 if( medianit ) sum = qmed_float( mc , exar ) ;
            else if( maxit    ) sum = max_float ( mc , exar ) ;
            else if( minit    ) sum = min_float ( mc , exar ) ;
            else if( sumit    ) sum *= mc ;
            else if( sumsqit  ) sum *= mc*mfac ; /* and include extra mfac */
            else if( percit   ){
              float *qar=(float *)malloc(sizeof(float)*mc) ; int iq ;
              memcpy(qar,exar,sizeof(float)*mc) ;
              qsort_float(mc,qar) ;
              iq = (int)rintf(0.01f*perc*mc) ; sum = qar[iq] ; free(qar) ;
            }

            sum = mfac * sum ;

            if( enormit ) sum = sqrt( sum ) ;

            if( dumpit ) printf("+++ %s = %g",sname,sum) ;
            else         printf("%g",sum) ;

            if( sigmait ){
               if( dumpit ) printf("  Sigma = %g",sigma) ;
               else         printf("  %g",sigma) ;
            }
            if( !quiet ) printf(" [%d voxels]\n",mc) ;
            else         printf("\n") ;
         }
         break ;

         case MRI_byte:{
            byte * bar = (byte *) DSET_ARRAY(input_dset,iv) ;
            double sum = 0.0 , sigma = 0.0 ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 || dumpit == 2 ) mfac = 1.0 ;

            if( dumpit ){
               int noscal = (dumpit==2) || (mfac==1.0) ;
               printf("+++ Dump for sub-brick %d:\n",iv) ;
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ){
                                                 if( noscal ) printf(" %d",bar[ii]) ;
                                                 else         printf(" %g",bar[ii]*mfac) ;

                                                 if( indump )
                                                    printf(" (%d,%d,%d)",
                                                           DSET_index_to_ix(input_dset,ii),
                                                           DSET_index_to_jy(input_dset,ii),
                                                           DSET_index_to_kz(input_dset,ii) ) ;
                                                 printf("\n") ;
                                              }
            }

            for( ii=mc=0 ; ii < nvox ; ii++ )
               if( GOOD(ii) ){
                  if( sumsqit ) sum += bar[ii]*bar[ii] ;
                  else          sum += bar[ii] ;
                  exar[mc++] = bar[ii] ;
               }
            if( mc > 0 ) sum = sum / mc ;

            if( sigmait && mc > 1 ){
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mc-1) ) ;
            }
                 if( medianit ) sum = qmed_float( mc , exar ) ;
            else if( maxit    ) sum = max_float ( mc , exar ) ;
            else if( minit    ) sum = min_float ( mc , exar ) ;
            else if( sumit    ) sum *= mc ;
            else if( sumsqit  ) sum *= mc*mfac ; /* and include extra mfac */
            else if( percit   ){
              float *qar=(float *)malloc(sizeof(float)*mc) ; int iq ;
              memcpy(qar,exar,sizeof(float)*mc) ;
              qsort_float(mc,qar) ;
              iq = (int)rintf(0.01f*perc*mc) ; sum = qar[iq] ; free(qar) ;
            }
            sum = mfac * sum ;

            if( enormit ) sum = sqrt( sum ) ;

            if( dumpit ) printf("+++ %s = %g",sname,sum) ;
            else         printf("%g",sum) ;

            if( sigmait ){
               if( dumpit ) printf("  Sigma = %g",sigma) ;
               else         printf("  %g",sigma) ;
            }
            if( !quiet ) printf(" [%d voxels]\n",mc) ;
            else         printf("\n") ;
         }
         break ;

         case MRI_float:{
            float * bar = (float *) DSET_ARRAY(input_dset,iv) ;
            double sum = 0.0 , sigma = 0.0 ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 || dumpit == 2 ) mfac = 1.0 ;

            if( dumpit ){
               int noscal = (dumpit==2) || (mfac==1.0) ;
               printf("+++ Dump for sub-brick %d:\n",iv) ;
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ){
                                                 if( noscal ) printf(" %g",bar[ii]) ;
                                                 else         printf(" %g",bar[ii]*mfac) ;

                                                 if( indump )
                                                    printf(" (%d,%d,%d)",
                                                           DSET_index_to_ix(input_dset,ii),
                                                           DSET_index_to_jy(input_dset,ii),
                                                           DSET_index_to_kz(input_dset,ii) ) ;
                                                 printf("\n") ;
                                              }
            }

            for( ii=mc=0 ; ii < nvox ; ii++ )
               if( GOOD(ii) ){
                  if( sumsqit ) sum += bar[ii]*bar[ii] ;
                  else          sum += bar[ii] ;
                  exar[mc++] = bar[ii] ;
               }
            if( mc > 0 ) sum = sum / mc ;

            if( sigmait && mc > 1 ){
               for( ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mc-1) ) ;
            }
                 if( medianit ) sum = qmed_float( mc , exar ) ;
            else if( maxit    ) sum = max_float ( mc , exar ) ;
            else if( minit    ) sum = min_float ( mc , exar ) ;
            else if( sumit    ) sum *= mc ;
            else if( sumsqit  ) sum *= mc*mfac ; /* and include extra mfac */
            else if( percit   ){
              float *qar=(float *)malloc(sizeof(float)*mc) ; int iq ;
              memcpy(qar,exar,sizeof(float)*mc) ;
              qsort_float(mc,qar) ;
              iq = (int)rintf(0.01f*perc*mc) ; sum = qar[iq] ; free(qar) ;
            }
            sum = mfac * sum ;

            if( enormit ) sum = sqrt( sum ) ;

            if( dumpit ) printf("+++ %s = %g",sname,sum) ;
            else         printf("%g",sum) ;

            if( sigmait ){
               if( dumpit ) printf("  Sigma = %g",sigma) ;
               else         printf("  %g",sigma) ;
            }
            if( !quiet ) printf(" [%d voxels]\n",mc) ;
            else         printf("\n") ;
         }
         break ;
      }
   }
   free(exar) ; DSET_delete(input_dset) ;
   exit(0) ;
}
