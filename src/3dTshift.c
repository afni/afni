/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*--------------------------------------------------------------------------*/

static void TS_copy_input_to_output(void) ;  /* prototype */

void TS_syntax(char * str)
{
   if( str != NULL ){
     ERROR_exit("%s",str) ; exit(1) ;
   }

   printf("Usage: 3dTshift [options] dataset\n"
          "Shifts voxel time series from the input dataset so that the separate\n"
          "slices are aligned to the same temporal origin.  By default, uses the\n"
          "slicewise shifting information in the dataset header (from the 'tpattern'\n"
          "input to program to3d).\n"
          "\n"
          "Method:  detrend -> interpolate -> retrend (optionally)\n"
          "\n"
          "The input dataset can have a sub-brick selector attached, as documented\n"
          "in '3dcalc -help'.\n"
          "\n"
          "The output dataset time series will be interpolated from the input to\n"
          "the new temporal grid.  This may not be the best way to analyze your\n"
          "data, but it can be convenient.\n"
          "\n"
          "Warnings:\n"
          "* Please recall the phenomenon of 'aliasing': frequencies above 1/(2*TR) can't\n"
          "  be properly interpolated.  For most 3D FMRI data, this means that cardiac\n"
          "  and respiratory effects will not be treated properly by this program.\n"
          "\n"
          "* The images at the beginning of a high-speed FMRI imaging run are usually\n"
          "  of a different quality than the later images, due to transient effects\n"
          "  before the longitudinal magnetization settles into a steady-state value.\n"
          "  These images should not be included in the interpolation!  For example,\n"
          "  if you wish to exclude the first 4 images, then the input dataset should\n"
          "  be specified in the form 'prefix+orig[4..$]'.  Alternatively, you can\n"
          "  use the '-ignore ii' option.\n"
          "\n"
          "* It seems to be best to use 3dTshift before using 3dvolreg.\n"
          "  (But this statement is controversial.)\n"
          "\n"
          "* If the input dataset does not have any slice timing information, and\n"
          "  '-tpattern' is not given, then this program just copies the input to\n"
          "  the output.  [02 Nov 2011 -- formerly, it failed]\n"
          "\n"
          "Options:\n"
          "  -verbose      = print lots of messages while program runs\n"
          "\n"
          "  -TR ddd       = use 'ddd' as the TR, rather than the value\n"
          "                  stored in the dataset header using to3d.\n"
          "                  You may attach the suffix 's' for seconds,\n"
          "                  or 'ms' for milliseconds.\n"
          "\n"
          "  -tzero zzz    = align each slice to time offset 'zzz';\n"
          "                  the value of 'zzz' must be between the\n"
          "                  minimum and maximum slice temporal offsets.\n"
          "            N.B.: The default alignment time is the average\n"
          "                  of the 'tpattern' values (either from the\n"
          "                  dataset header or from the -tpattern option)\n"
          "\n"
          "  -slice nnn    = align each slice to the time offset of slice\n"
          "                  number 'nnn' - only one of the -tzero and\n"
          "                  -slice options can be used.\n"
          "\n"
          "  -prefix ppp   = use 'ppp' for the prefix of the output file;\n"
          "                  the default is 'tshift'.\n"
          "\n"
          "  -ignore ii    = Ignore the first 'ii' points. (Default is ii=0.)\n"
          "                  The first ii values will be unchanged in the output\n"
          "                  (regardless of the -rlt option).  They also will\n"
          "                  not be used in the detrending or time shifting.\n"
          "\n"
          "  -rlt          = Before shifting, the mean and linear trend\n"
          "  -rlt+         = of each time series is removed.  The default\n"
          "                  action is to add these back in after shifting.\n"
          "                  -rlt  means to leave both of these out of the output\n"
          "                  -rlt+ means to add only the mean back into the output\n"
          "                  (cf. '3dTcat -help')\n"
          "\n"
          "  -no_detrend   = Do not remove or restore linear trend.\n"
          "                  Heptic becomes the default interpolation method.\n"
          "\n"
          "  -Fourier = Use a Fourier method (the default: most accurate; slowest).\n"
          "  -linear  = Use linear (1st order polynomial) interpolation (least accurate).\n"
          "  -cubic   = Use the cubic (3rd order) Lagrange polynomial interpolation.\n"
          "  -quintic = Use the quintic (5th order) Lagrange polynomial interpolation.\n"
          "  -heptic  = Use the heptic (7th order) Lagrange polynomial interpolation.\n"
          "\n"
          "  -tpattern ttt = use 'ttt' as the slice time pattern, rather\n"
          "                  than the pattern in the input dataset header;\n"
          "                  'ttt' can have any of the values that would\n"
          "                  go in the 'tpattern' input to to3d, described below:\n"
          "\n"
          "   alt+z = altplus   = alternating in the plus direction\n"
          "   alt+z2            = alternating, starting at slice #1 instead of #0\n"
          "   alt-z = altminus  = alternating in the minus direction\n"
          "   alt-z2            = alternating, starting at slice #nz-2 instead of #nz-1\n"
          "   seq+z = seqplus   = sequential in the plus direction\n"
          "   seq-z = seqminus  = sequential in the minus direction\n"
          "   @filename         = read temporal offsets from 'filename'\n"
          "\n"
          "  For example if nz = 5 and TR = 1000, then the inter-slice\n"
          "  time is taken to be dt = TR/nz = 200.  In this case, the\n"
          "  slices are offset in time by the following amounts:\n"
          "\n"
          "             S L I C E   N U M B E R\n"
          "   tpattern    0   1   2   3   4   Comment\n"
          "   --------- --- --- --- --- ---   -------------------------------\n"
          "   altplus     0 600 200 800 400   Alternating in the +z direction\n"
          "   alt+z2    400   0 600 200 800   Alternating, but starting at #1\n"
          "   altminus  400 800 200 600   0   Alternating in the -z direction\n"
          "   alt-z2    800 200 600   0 400   Alternating, starting at #nz-2 \n"
          "   seqplus     0 200 400 600 800   Sequential  in the +z direction\n"
          "   seqminus  800 600 400 200   0   Sequential  in the -z direction\n"
          "\n"
          "  If @filename is used for tpattern, then nz ASCII-formatted numbers\n"
          "  are read from the file.  These indicate the time offsets for each\n"
          "  slice. For example, if 'filename' contains\n"
          "     0 600 200 800 400\n"
          "  then this is equivalent to 'altplus' in the above example.\n"
          "  (nz = number of slices in the input dataset)\n"
          "\n"
          "N.B.: if you are using -tpattern, make sure that the units supplied\n"
          "      match the units of TR in the dataset header, or provide a\n"
          "      new TR using the -TR option.\n"
          "\n"
          "As a test of how well 3dTshift interpolates, you can take a dataset\n"
          "that was created with '-tpattern alt+z', run 3dTshift on it, and\n"
          "then run 3dTshift on the new dataset with '-tpattern alt-z' -- the\n"
          "effect will be to reshift the dataset back to the original time\n"
          "grid.  Comparing the original dataset to the shifted-then-reshifted\n"
          "output will show where 3dTshift does a good job and where it does\n"
          "a bad job.\n"
          "\n"
          "******* Voxel-Wise Shifting -- New Option [Sep 2011] *******\n"
          "\n"
          " -voxshift fset = Read in dataset 'fset' and use the values in there\n"
          "                  to shift each input dataset's voxel's time series a\n"
          "                  different amount.  The values in 'fset' are NOT in\n"
          "                  units of time, but rather are fractions of a TR\n"
          "                  to shift -- a positive value means to shift backwards.\n"
          "                 * To compute an fset-style dataset that matches the\n"
          "                   time pattern of an existing dataset, try\n"
          "       set TR = 2.5\n"
          "       3dcalc -a 'dset+orig[0..1]' -datum float -prefix Toff -expr \"t/${TR}-l\"\n"
          "                   where you first set the shell variable TR to the true TR\n"
          "                   of the dataset, then create a dataset Toff+orig with the\n"
          "                   fractional shift of each slice stored in each voxel.  Then\n"
          "                   the two commands below should give identical outputs:\n"
          "       3dTshift -ignore 2 -tzero 0 -prefix Dold -heptic dset+orig\n"
          "       3dTshift -ignore 2 -voxshift Toff+orig -prefix Dnew -heptic dset+orig\n"
          "\n"
          " Use of '-voxshift' means that options such as '-tzero' and '-tpattern' are\n"
          " ignored -- the burden is on you to encode all the shifts into the 'fset'\n"
          " dataset somehow.  (3dcalc can be your friend here.)\n"
          "\n"
          "-- RWCox - 31 October 1999\n"
        ) ;

   printf("\n" MASTER_SHORTHELP_STRING ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
}

/*-----------------------------------------------------------------------------*/

static float  TS_TR     = 0.0 ;
static int    TS_tunits = UNITS_SEC_TYPE ;
static float *TS_tpat   = NULL ;
static float  TS_tzero  = -1.0 ;
static int    TS_slice  = -1 ;
static int    TS_rlt    = 0 ;   /* 0=add both in; 1=add neither; 2=add mean */

static int    TS_detrend = 1 ;  /* do any detrend?  3 Jan 2007 [rickr] */
static int    TS_verbose = 0 ;

static int    TS_ignore  = 0 ;  /* 15 Feb 2001 */

static THD_3dim_dataset *TS_dset = NULL , *TS_oset = NULL ;

static THD_3dim_dataset *TS_fset = NULL ; /* 21 Sep 2011 */

static char *TS_tpattern = NULL ;

static char TS_prefix[THD_MAX_PREFIX] = "tshift" ;

/*-----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int nopt=1 ;
   int nzz, ii,jj,kk , ntt,nxx,nyy,nxy , nup ;
   float tomax,tomin , fmin,fmax , gmin=0.0,gmax=0.0 , f0,f1 , g0,g1 ;
   float ffmin=0.0,ffmax=0.0 , ggmin=0.0,ggmax=0.0 , fshift=0.0,gshift=0.0 ;
   MRI_IMAGE *flim , *glim=NULL ;
   float *far , *gar ;
   int ignore=0 , BAD=0 ;

   /*- scan command line -*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ) TS_syntax(NULL) ;

   mainENTRY("3dTshift main"); machdep(); AFNI_logger("3dTshift",argc,argv);
   PRINT_VERSION("3dTshift");

   SHIFT_set_method( MRI_FOURIER ) ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-voxshift") == 0 ){  /* 21 Sep 2011 */
        if( TS_fset != NULL ) ERROR_exit("Can't use -voxshift twice!") ;
        TS_fset = TS_fset = THD_open_dataset( argv[++nopt] ) ;
        if( TS_fset == NULL ) TS_syntax("Can't open -voxshift dataset!") ;
        DSET_load(TS_fset) ; CHECK_LOAD_ERROR(TS_fset) ;
        nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-BAD") == 0 ){
        BAD = 1 ; nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-verbose",5) == 0 ){
         TS_verbose++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-ignore") == 0 ){  /* 15 Feb 2001 */
         TS_ignore = (int) strtod(argv[++nopt],NULL) ;
         if( TS_ignore < 0 ) TS_syntax("-ignore value is negative!") ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-Fourier",4) == 0 || strncmp(argv[nopt],"-fourier",4) == 0 ){
         SHIFT_set_method( MRI_FOURIER ) ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-cubic",4) == 0 || strncmp(argv[nopt],"-Cubic",4) == 0 ){
         SHIFT_set_method( MRI_CUBIC ) ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-quintic",4) == 0 || strncmp(argv[nopt],"-Quintic",4) == 0 ){
         SHIFT_set_method( MRI_QUINTIC ) ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-heptic",4) == 0 || strncmp(argv[nopt],"-Heptic",4) == 0 ){
         SHIFT_set_method( MRI_HEPTIC ) ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-linear",4) == 0 || strncmp(argv[nopt],"-Linear",4) == 0 ){
         SHIFT_set_method( MRI_LINEAR ) ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-TR") == 0 ){
         char * eptr ;
         if( ++nopt >= argc ) TS_syntax("-TR needs an argument!") ;
         TS_TR = strtod( argv[nopt] , &eptr ) ;
         if( TS_TR <= 0.0 ) TS_syntax("illegal value after -TR!") ;

         if( strcmp(eptr,"ms")==0 || strcmp(eptr,"msec")==0 ){
            TS_tunits = UNITS_MSEC_TYPE ;
            WARNING_message("TR expressed in milliseconds is deprecated.") ;
         } else if( strcmp(eptr,"s")==0 || strcmp(eptr,"sec")==0 ){
            TS_tunits = UNITS_SEC_TYPE ;
         } else if( strcmp(eptr,"Hz")==0 || strcmp(eptr,"Hertz")==0 ){
            TS_tunits = UNITS_HZ_TYPE ;
         }

         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-tzero") == 0 ){
         if( ++nopt >= argc ) TS_syntax("-tzero needs an argument!") ;
         TS_tzero = strtod( argv[nopt] , NULL ) ;
         if( TS_tzero < 0.0 ) TS_syntax("illegal value after -tzero!") ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-slice") == 0 ){
         if( ++nopt >= argc ) TS_syntax("-slice needs an argument!") ;
         TS_slice = strtod( argv[nopt] , NULL ) ;
         if( TS_slice < 0 ) TS_syntax("illegal value after -slice!") ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ) TS_syntax("-prefix needs an argument!") ;
         MCW_strncpy( TS_prefix , argv[nopt] , THD_MAX_PREFIX ) ;
         if( !THD_filename_ok(TS_prefix) ) TS_syntax("illegal value after -prefix") ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-rlt") == 0 ){
         if( !TS_detrend ) TS_syntax("cannot use both -rlt and -no_detrend");
         TS_rlt = 1 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-rlt+") == 0 ){
         if( !TS_detrend ) TS_syntax("cannot use both -rlt+ and -no_detrend");
         TS_rlt = 2 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-no_detrend") == 0 ){ /* 3 Jan 2007 */
         if( TS_rlt ) TS_syntax("cannot use both -rlt and -no_detrend");
         if( SHIFT_get_method() == MRI_FOURIER ){
            fprintf(stderr,"found -no_detrend, changing default to -heptic\n");
            SHIFT_set_method(MRI_HEPTIC);
         }
         TS_detrend = 0 ;
         TS_rlt = 2 ;        /* still de-mean/re-mean, as Bob suggests */
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-tpattern") == 0 ){
         if( ++nopt >= argc ) TS_syntax("-tpattern needs an argument!") ;
         TS_tpattern = argv[nopt] ;
         nopt++ ; continue ;
      }

      ERROR_exit("Unknown option: %s",argv[nopt]) ;

   }  /* end of scan command line */

   if( TS_detrend == 0 && SHIFT_get_method() == MRI_FOURIER )
     WARNING_message("-no_detrend with Fourier interpolation is dangerous");

   /*- open dataset; extract values, check for errors -*/

   if( nopt >= argc ) TS_syntax("Need a dataset input?!") ;
   if( TS_verbose ) printf("++ opening input dataset header\n") ;
   TS_dset = THD_open_dataset( argv[nopt] ) ;
   if( TS_dset == NULL ) TS_syntax("Can't open input dataset!") ;

   nxx = DSET_NX(TS_dset) ;                      /* get dimensions */
   nyy = DSET_NY(TS_dset) ; nxy = nxx * nyy ;
   nzz = DSET_NZ(TS_dset) ;
   ntt = DSET_NVALS(TS_dset) ;

   if( DSET_NVALS(TS_dset) < 2 ){
     WARNING_message("Input dataset has only 1 value per voxel!") ;
     TS_copy_input_to_output() ;
   }

   if( TS_slice >= nzz ) TS_syntax("-slice value is too large") ;

   if( TS_ignore > ntt-5 ) TS_syntax("-ignore value is too large") ;

   if( TS_TR <= 0.0 ){                                    /* set TR from dataset */
      if( TS_dset->taxis != NULL ){
        TS_TR     = DSET_TIMESTEP(TS_dset) ;
        TS_tunits = TS_dset->taxis->units_type ;
      }
      if( TS_TR <= 0.0 ){
        TS_TR     = 1.0f ;
        TS_tunits = UNITS_SEC_TYPE ;
      }
      if( TS_verbose )
        printf("++ using dataset TR = %g %s\n",TS_TR,UNITS_TYPE_LABEL(TS_tunits)) ;
   }

   if( TS_fset != NULL ){

     if( nxx != DSET_NX(TS_fset) ||
         nyy != DSET_NY(TS_fset) ||
         nzz != DSET_NZ(TS_fset)   )
       TS_syntax("-voxshift and input datasets don't match!") ;

   } else {

     if( TS_dset->taxis == NULL ){
       if( TS_TR == 0.0 || TS_tpattern == NULL ){
         WARNING_message("dataset has no time axis!") ;
         TS_copy_input_to_output() ;
       }
     } else if( TS_tpattern == NULL && TS_dset->taxis->toff_sl == NULL ){
       WARNING_message("dataset is already aligned in time!") ;
       TS_copy_input_to_output() ;
     }

     if( TS_tpattern != NULL ){                                    /* set pattern */
       TS_tpat = TS_parse_tpattern( nzz , TS_TR , TS_tpattern ) ;
     } else {
       if( TS_dset->taxis->nsl != nzz ){
         WARNING_message("dataset temporal pattern is malformed!") ; /* should not happen */
         TS_copy_input_to_output() ;
       }

       TS_tpat = (float *) malloc( sizeof(float) * nzz ) ;
       memcpy( TS_tpat , TS_dset->taxis->toff_sl , sizeof(float)*nzz ) ;
     }
     if( TS_verbose ){
       printf("++ using tpattern = ") ;
       for( ii=0 ; ii < nzz ; ii++ ) printf("%g ",TS_tpat[ii]) ;
       printf("%s\n",UNITS_TYPE_LABEL(TS_tunits)) ;
     }

     tomin = WAY_BIG ; tomax = -WAY_BIG ;                      /* check pattern */
     for( ii=0 ; ii < nzz ; ii++ ){
        if( TS_tpat[ii] > tomax ) tomax = TS_tpat[ii] ;
        if( TS_tpat[ii] < tomin ) tomin = TS_tpat[ii] ;
     }
     if( tomin < 0.0 || tomax > TS_TR ){
       WARNING_message("some value in tpattern is outside range 0..TR=%g",TS_TR) ;
       TS_copy_input_to_output() ;
     } else if( tomin >= tomax ){
       WARNING_message("temporal pattern is already aligned in time!") ;
       TS_copy_input_to_output() ;
     }


     if( TS_slice >= 0 && TS_slice < nzz ){                   /* set common time point */
       TS_tzero = TS_tpat[TS_slice] ;
     } else if( TS_tzero < 0.0 ){
       TS_tzero = 0.0 ;
       for( ii=0 ; ii < nzz ; ii++ ) TS_tzero += TS_tpat[ii] ;
       TS_tzero /= nzz ;
     }
     if( TS_verbose ) printf("++ common time point set to %g\n",TS_tzero) ;

   }

   /*- copy input dataset, modify it to be the output -*/

   if( TS_verbose ) printf("++ copying input dataset bricks\n") ;

   TS_oset = EDIT_full_copy( TS_dset , TS_prefix ) ;
   if( TS_oset == NULL ) TS_syntax("Can't copy input dataset!") ;
   DSET_unload( TS_dset ) ;

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(TS_oset)) )
     TS_syntax("output dataset already exists!") ;

   tross_Copy_History( TS_dset , TS_oset ) ;
   tross_Make_History( "3dTshift" , argc,argv , TS_oset ) ;

   /*- reconfigure the time axis -*/

   EDIT_dset_items( TS_oset ,
                       ADN_ntt    , ntt       ,  /* in case not already set */
                       ADN_ttdel  , TS_TR     ,  /* may have changed */
                       ADN_tunits , TS_tunits ,  /* may have changed */
                       ADN_nsl    , 0         ,  /* will have no offsets when done */
#if 0
                       ADN_ttorg  , 0.0       ,  /* in case not already set */
                       ADN_ttdur  , 0.0       ,  /* in case not already set */
#endif
                    ADN_none ) ;

   /*---- do the temporal shifting! ----*/

   nup = csfft_nextup_one35( ntt+4 ) ;
   ignore = TS_ignore ;

   if( TS_verbose && SHIFT_get_method() == MRI_FOURIER )
      printf("++ Time series length = %d; FFT length set to %d\n",ntt,nup) ;

   for( kk=0 ; kk < nzz ; kk++ ){       /* loop over slices */

      if( TS_fset == NULL ){
        fshift = (TS_tzero - TS_tpat[kk]) / TS_TR ;    /* rightward fractional shift */
#if 1
        if( !BAD ) fshift = -fshift ;   /* 24 Apr 2003 -- OOG */
#endif
        gshift = fshift ;

        if( TS_verbose )
          printf("++ slice %d: fractional shift = %g\n",kk,fshift) ;

        /* if removing any trend, process the slice       22 May 2008 [rickr] */
        if( fabs(fshift) < 0.001 && ! TS_rlt ) continue ;  /* skip this slice */
      }

      for( ii=0 ; ii < nxy ; ii+=2 ){   /* loop over voxel pairs in slice */

         flim = THD_extract_series( ii+kk*nxy , TS_oset , 0 ) ;  /* get this voxel */
         far  = MRI_FLOAT_PTR(flim) ;

         if( TS_fset != NULL ){  /* 21 Sep 2011 */
           MRI_IMAGE *qim = THD_extract_series( ii+kk*nxy , TS_fset , 0 ) ;
           float *qar = MRI_FLOAT_PTR(qim) ; fshift = qar[0] ; mri_free(qim) ;
         }

         if( TS_rlt == 0 ){
            for( ffmin=ffmax=far[ignore],jj=ignore+1 ; jj < ntt ; jj++ ){
                    if( far[jj] < ffmin ) ffmin = far[jj] ;
               else if( far[jj] > ffmax ) ffmax = far[jj] ;
            }
         }

         if( TS_detrend )   /* remove trend, else mean   3 Jan 2007 [rickr] */
            THD_linear_detrend( ntt-ignore , far+ignore , &f0,&f1 ) ;
         else
            THD_const_detrend( ntt-ignore , far+ignore , &f0 ) ;

         for( fmin=fmax=far[ignore],jj=ignore+1 ; jj < ntt ; jj++ ){
                 if( far[jj] < fmin ) fmin = far[jj] ;   /* range of data: after */
            else if( far[jj] > fmax ) fmax = far[jj] ;
         }

         if( ii < nxy-1 ){                                       /* get next voxel */
            glim = THD_extract_series( ii+kk*nxy+1 , TS_oset , 0 ) ;
            gar  = MRI_FLOAT_PTR(glim) ;

            if( TS_fset != NULL ){  /* 21 Sep 2011 */
              MRI_IMAGE *qim = THD_extract_series( ii+kk*nxy+1 , TS_fset , 0 ) ;
              float *qar = MRI_FLOAT_PTR(qim) ; gshift = qar[0] ; mri_free(qim) ;
            }

            if( TS_rlt == 0 ){
               for( ggmin=ggmax=gar[ignore],jj=ignore+1 ; jj < ntt ; jj++ ){
                       if( gar[jj] < ggmin ) ggmin = gar[jj] ;
                  else if( gar[jj] > ggmax ) ggmax = gar[jj] ;
               }
            }

            if( TS_detrend )
               THD_linear_detrend( ntt-ignore , gar+ignore , &g0,&g1 ) ;
            else
               THD_const_detrend( ntt-ignore , far+ignore , &g0 ) ;

            for( gmin=gmax=gar[ignore],jj=ignore+1 ; jj < ntt ; jj++ ){
                    if( gar[jj] < gmin ) gmin = gar[jj] ;
               else if( gar[jj] > gmax ) gmax = gar[jj] ;
            }
         } else {
            gar  = NULL ;
         }

         if( gar != NULL )
            SHIFT_two_rows( ntt-ignore,nup, fshift,far+ignore , gshift, gar+ignore ) ;
         else
            SHIFT_two_rows( ntt-ignore,nup, fshift,far+ignore , gshift, NULL ) ;

         for( jj=ignore ; jj < ntt ; jj++ ){
                 if( far[jj] < fmin ) far[jj] = fmin ;           /* clip to input range */
            else if( far[jj] > fmax ) far[jj] = fmax ;
            switch( TS_rlt ){                                    /* restore trend? */
               case 0:
                  far[jj] += (f0 + (jj-ignore)*f1) ;
                       if( far[jj] < ffmin ) far[jj] = ffmin ;
                  else if( far[jj] > ffmax ) far[jj] = ffmax ;
               break ;

               case 2:
                  far[jj] += f0 ;
               break ;
            }
         }

         if( gar != NULL ){
            for( jj=ignore ; jj < ntt ; jj++ ){
                    if( gar[jj] < gmin ) gar[jj] = gmin ;
               else if( gar[jj] > gmax ) gar[jj] = gmax ;
               switch( TS_rlt ){
                  case 0:
                     gar[jj] += (g0 + (jj-ignore)*g1) ;
                          if( gar[jj] < ggmin ) gar[jj] = ggmin ;
                     else if( gar[jj] > ggmax ) gar[jj] = ggmax ;
                  break ;

                  case 2:
                     gar[jj] += g0 ;
                  break ;
               }
            }
         }

         /* put back into dataset */

         THD_insert_series( ii+kk*nxy , TS_oset , ntt , MRI_float , far , 0 ) ;
         if( gar != NULL )
            THD_insert_series( ii+kk*nxy+1 , TS_oset , ntt , MRI_float , gar , 0 ) ;

         /* throw out the trash */

         mri_free(flim) ; if( gar != NULL ) mri_free(glim) ;
      }
   }

   if( TS_fset != NULL ) DSET_delete( TS_fset ) ;

   DSET_write( TS_oset ) ;
   if( TS_verbose ) fprintf(stderr,"++ Wrote output: %s\n",DSET_BRIKNAME(TS_oset)) ;
   exit(0) ;
}

/*-----------------------------------------------------------------------------*/

static void TS_copy_input_to_output(void)
{
   WARNING_message("==>> output dataset is just a copy of input dataset") ;
   TS_oset = EDIT_full_copy( TS_dset , TS_prefix ) ;
   if( TS_oset == NULL ) TS_syntax("Can't copy input dataset!") ;
   DSET_unload( TS_dset ) ;
   DSET_write( TS_oset ) ;
   if( TS_verbose ) fprintf(stderr,"++ Wrote output: %s\n",DSET_BRIKNAME(TS_oset)) ;
   exit(0) ;
}
