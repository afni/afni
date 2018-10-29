#include "mrilib.h"

#if 0
extern void mri_FWHM_1dif_moments( MRI_IMAGE *im , byte *mask ) ;
extern void THD_estimate_FWHM_moments_all( THD_3dim_dataset *dset,
                                    byte *mask, int demed , int unif ) ;
#endif

#ifdef USE_OMP
# include <omp.h>
# include "mri_fwhm.c"
#else
extern void mri_fwhm_mom12_set_stdev_fac(double) ;
#endif

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL ; char *inset_prefix , *cpp ;
   int iarg=1 , ii , nvals,nvox , ncon ;
   MRI_IMAGE *outim ; float *outar ;
   byte *mask=NULL ; int mask_nx=0,mask_ny=0,mask_nz=0 , automask=0 ;
   char *outfile = NULL ;
   double fx,fy,fz , cx,cy,cz , ccomb ; int nx,ny,nz , ncomb ;
   int geom=1 , demed=0 , unif=0 , corder=0 , combine=0 ;
   char *newprefix=NULL ;
   int do_acf = -1 ; float acf_rad=0.0f ; int do_classic=0 ; int addcol5=0 ;
   char *acf_fname="3dFWHMx.1D" ; MRI_IMAGE *acf_im=NULL ; float_quad acf_Epar ;
   double ct ;

   /*---- for the clueless who wish to become clueful ----*/

   AFNI_SETUP_OMP(0) ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dFWHMx [options] dataset\n"
      "\n"
      "**** NOTICE ****\n"
      " You should use the '-acf' option (which is what afni_proc.py uses now).\n"
      " The 'Classic' method giving just a Gaussian FWHM can no longer be\n"
      " considered reliable for FMRI statistical analyses!\n"
      "****************\n"
      "\n"
      " >>>>> 20 July 2017: Results from the 'Classic' method are no longer output!\n"
      " >>>>>               If you want to see these values, you must give the\n"
      " >>>>>               command line option '-ShowMeClassicFWHM'.\n"
      " >>>>>               You no longer need to give the '-acf' option, as it\n"
      " >>>>>               is now the default method of calculation (and\n"
      " >>>>>               cannot be turned off). Note that if you need the\n"
      " >>>>>               FWHM estimate, the '-acf' method gives a value\n"
      " >>>>>               for that as its fourth output.\n"
      " >>>>> Options and comments that only apply to the 'Classic' FWHM estimation\n"
      " >>>>> method are now marked below with this '>>>>>' marker, to indicate that\n"
      " >>>>> they are obsolete, archaic, and endangered (as well as fattening).\n"
      "\n"
#if 1
      ">>>>> Unlike the older 3dFWHM, this program computes FWHMs for all sub-bricks\n"
      ">>>>> in the input dataset, each one separately.  The output for each one is\n"
      ">>>>> written to the file specified by '-out'.  The mean (arithmetic or geometric)\n"
      ">>>>> of all the FWHMs along each axis is written to stdout.  (A non-positive\n"
      ">>>>> output value indicates something bad happened; e.g., FWHM in z is meaningless\n"
      ">>>>> for a 2D dataset; the estimation method computed incoherent intermediate results.)\n"
#endif
      "\n"
      "(Classic) METHOD: <<<<< NO LONGER OUTPUT -- SEE ABOVE >>>>>\n"
      " - Calculate ratio of variance of first differences to data variance.\n"
      " - Should be the same as 3dFWHM for a 1-brick dataset.\n"
      "   (But the output format is simpler to use in a script.)\n"
      "\n"
      "**----------------------------------------------------------------------------**\n"
      "************* IMPORTANT NOTE [Dec 2015] ****************************************\n"
      "**----------------------------------------------------------------------------**\n"
      "A completely new method for estimating and using noise smoothness values is\n"
      "now available in 3dFWHMx and 3dClustSim. This method is implemented in the\n"
      "'-acf' options to both programs.  'ACF' stands for (spatial) AutoCorrelation\n"
      "Function, and it is estimated by calculating moments of differences out to\n"
      "a larger radius than before.\n"
      "\n"
      "Notably, real FMRI data does not actually have a Gaussian-shaped ACF, so the\n"
      "estimated ACF is then fit (in 3dFWHMx) to a mixed model (Gaussian plus\n"
      "mono-exponential) of the form\n"
      "  ACF(r) = a * exp(-r*r/(2*b*b)) + (1-a)*exp(-r/c)\n"
      "where 'r' is the radius, and 'a', 'b', 'c' are the fitted parameters.\n"
      "The apparent FWHM from this model is usually somewhat larger in real data\n"
      "than the FWHM estimated from just the nearest-neighbor differences used\n"
      "in the 'classic' analysis.\n"
      "\n"
      "The longer tails provided by the mono-exponential are also significant.\n"
      "3dClustSim has also been modified to use the ACF model given above to generate\n"
      "noise random fields.\n"
      "\n"
      "**----------------------------------------------------------------------------**\n"
      "** The take-away (TL;DR or summary) message is that the 'classic' 3dFWHMx and **\n"
      "** 3dClustSim analysis, using a pure Gaussian ACF, is not very correct for    **\n"
      "** FMRI data -- I cannot speak for PET or MEG data.                           **\n"
#if 0
      "**                                                                            **\n"
      "** You should start using  the '-acf' options in your own scripts.  AFNI      **\n"
      "** scripts from afni_proc.py are moving away from the 'classic' method.  At   **\n"
      "** some point in the future, '-acf' will become the default in both programs, **\n"
      "** and you will have to actively specify '-classic' to use the older method.  **\n"
#endif
      "**----------------------------------------------------------------------------**\n"
      "\n"
      "OPTIONS:\n"
      "  -mask mmm   = Use only voxels that are nonzero in dataset 'mmm'.\n"
      "  -automask   = Compute a mask from THIS dataset, a la 3dAutomask.\n"
      "                [Default = use all voxels]\n"
      "\n"
      "  -input ddd }=\n"
      "    *OR*     }= Use dataset 'ddd' as the input.\n"
      "  -dset  ddd }=\n"
      "\n"
      "  -demed      = If the input dataset has more than one sub-brick\n"
      "                (e.g., has a time axis), then subtract the median\n"
      "                of each voxel's time series before processing FWHM.\n"
      "                This will tend to remove intrinsic spatial structure\n"
      "                and leave behind the noise.\n"
      "                [Default = don't do this]\n"
      "  -unif       = If the input dataset has more than one sub-brick,\n"
      "                then normalize each voxel's time series to have\n"
      "                the same MAD before processing FWHM.  Implies -demed.\n"
      "                [Default = don't do this]\n"
      "  -detrend [q]= Instead of demed (0th order detrending), detrend to\n"
      "                order 'q'.  If q is not given, the program picks q=NT/30.\n"
      "                -detrend disables -demed, and includes -unif.\n"
      "        **N.B.: I recommend this option IF you are running 3dFWHMx on\n"
      "                functional MRI time series that have NOT been processed\n"
      "                to remove any activation and/or physiological artifacts.\n"
      "           **** If you are running 3dFWHMx on the residual (errts) time\n"
      "                series from afni_proc.py, you don't need -detrend.\n"
      "        **N.B.: This is the same detrending as done in 3dDespike;\n"
      "                using 2*q+3 basis functions for q > 0.\n"
      "        ******* If you don't use '-detrend', the program checks\n"
      "                if a large number of voxels are have significant\n"
      "                nonzero means. If so, the program will print a warning\n"
      "                message suggesting the use of '-detrend', since inherent\n"
      "                spatial structure in the image will bias the estimation\n"
      "                of the FWHM of the image time series NOISE (which is usually\n"
      "                the point of using 3dFWHMx).\n"
      "  -detprefix d= Save the detrended file into a dataset with prefix 'd'.\n"
      "                Used mostly to figure out what the hell is going on,\n"
      "                when strange results transpire.\n"
      "\n"
      ">>>>>\n"
      "  -geom      }= If the input dataset has more than one sub-brick,\n"
      "    *OR*     }= compute the final estimate as the geometric mean\n"
      "  -arith     }= or the arithmetic mean of the individual sub-brick\n"
      "                FWHM estimates. [Default = -geom, for no good reason]\n"
      "\n"
      ">>>>>\n"
      "  -combine    = combine the final measurements along each axis into\n"
      "                one result\n"
      "\n"
      ">>>>>\n"
      "  -out ttt    = Write output to file 'ttt' (3 columns of numbers).\n"
      "                If not given, the sub-brick outputs are not written.\n"
      "                Use '-out -' to write to stdout, if desired.\n"
      "\n"
      ">>>>>\n"
      "  -compat     = Be compatible with the older 3dFWHM, where if a\n"
      "                voxel is in the mask, then its neighbors are used\n"
      "                for differencing, even if they are not themselves in\n"
      "                the mask.  This was an error; now, neighbors must also\n"
      "                be in the mask to be used in the differencing.\n"
      "                Use '-compat' to use the older method.\n"
      "             ** NOT RECOMMENDED except for comparison purposes! **\n"
      "\n"
      "  -ACF [anam] = ** new option Nov 2015 **\n"
      "   *or*         The '-ACF' option computes the spatial autocorrelation\n"
      "  -acf [anam]   of the data as a function of radius, then fits that\n"
      "                to a model of the form\n"
      "                  ACF(r) = a * exp(-r*r/(2*b*b)) + (1-a)*exp(-r/c)\n"
      "                and outputs the 3 model parameters (a,b,c) to stdout.\n"
      "              * The model fit assumes spherical symmetry in the ACF.\n"
      "              * The results shown on stdout are in the format\n"
      "          >>>>> The first 2 lines below will only be output <<<<<\n"
      "          >>>>> if you use the option '-ShowMeClassicFWHM'. <<<<<\n"
      "          >>>>> Otherwise, the 'old-style' FWHM values will <<<<<\n"
      "          >>>>> show up as all zeros (0 0 0 0).             <<<<<\n"
      "  # old-style FWHM parameters\n"
      "   10.4069  10.3441  9.87341     10.2053\n"
      "  # ACF model parameters for a*exp(-r*r/(2*b*b))+(1-a)*exp(-r/c) plus effective FWHM\n"
      "   0.578615  6.37267  14.402     16.1453\n"
      "                The lines that start with '#' are comments.\n"
      "          >>>>> The first numeric line contains the 'old style' FWHM estimates,\n"
      "          >>>>>   FWHM_x FWHM_y FHWM_z  FWHM_combined\n"
      "                The second numeric line contains the a,b,c parameters, plus the\n"
      "                combined estimated FWHM from those parameters.  In this example,\n"
      "                the fit was about 58%% Gaussian shape, 42%% exponential shape,\n"
      "                and the effective FWHM from this fit was 16.14mm, versus 10.21mm\n"
      "                estimated in the 'old way'.\n"
      "              * If you use '-acf' instead of '-ACF', then the comment #lines\n"
      "                in the stdout information will be omitted.  This might help\n"
      "                in parsing the output inside a script.\n"
      "              * The empirical ACF results are also written to the file\n"
#ifdef ADD_COL5
      "                'anam' in 5 columns:\n"
      "                   radius ACF(r) model(r) gaussian_NEWmodel(r) gaussian_OLDmodel(r)\n"
      "                where 'gaussian_NEWmodel' is the Gaussian with the FHWM estimated\n"
      "                from the ACF, and 'gaussian_OLDmodel' is with the FWHM estimated\n"
      "                via the 'classic' (Forman 1995) method.\n"
#else
      "                'anam' in 4 columns:\n"
      "                   radius ACF(r) model(r) gaussian_NEWmodel(r)(r)\n"
      "                where 'gaussian_NEWmodel' is the Gaussian with the FWHM estimated\n"
      "                from the ACF, NOT via the 'classic' (Forman 1995) method.\n"
#endif
      "              * If 'anam' is not given (that is, another option starting\n"
      "                with '-' immediately follows '-acf'), then '3dFWHMx.1D' will\n"
      "                be used for this filename. If 'anam' is set to 'NULL', then\n"
      "                the corresponding output files will not be saved.\n"
      "              * By default, the ACF is computed out to a radius based on\n"
      "                a multiple of the 'classic' FWHM estimate.  If you want to\n"
      "                specify that radius (in mm), you can put that value after\n"
      "                the 'anam' parameter, as in '-acf something.1D 40.0'.\n"
      "              * In addition, a graph of these functions will be saved\n"
      "                into file 'anam'.png, for your pleasure and elucidation.\n"
      "              * Note that the ACF calculations are slower than the\n"
      "                'classic' FWHM calculations.\n"
      "                To reduce this sloth, 3dFWHMx now uses OpenMP to speed things up.\n"
#ifndef USE_OMP
      "                (: Unfortunately, this version was NOT compiled to use OpenMP :)\n"
#endif
      "              * The ACF modeling is intended to enhance 3dClustSim, and\n"
      "                may or may not be useful for any other purpose!\n"
      "\n"
      ">>>>> SAMPLE USAGE: (tcsh)\n"
      ">>>>>   set zork = ( `3dFWHMx -automask -input junque+orig` )\n"
      ">>>>> Captures the FWHM-x, FWHM-y, FWHM-z values into shell variable 'zork'.\n"
      "\n"
      "INPUT FILE RECOMMENDATIONS:\n"
      "* For FMRI statistical purposes, you DO NOT want the FWHM or ACF to reflect\n"
      "  any spatial structure of the underlying anatomy.  Rather, you want\n"
      "  the FWHM/ACF to reflect the spatial structure of the NOISE.  This means\n"
      "  that the input dataset should not have anatomical (spatial) structure.\n"
      "* One good form of input is the output of '3dDeconvolve -errts', which is\n"
      "  the dataset of residuals left over after the GLM fitted signal model is\n"
      "  subtracted out from each voxel's time series.\n"
      "* If you don't want to go to that much trouble, use '-detrend' to approximately\n"
      "  subtract out the anatomical spatial structure, OR use the output of 3dDetrend\n"
      "  for the same purpose.\n"
      "* If you do not use '-detrend', the program attempts to find non-zero spatial\n"
      "  structure in the input, and will print a warning message if it is detected.\n"
      "\n"
      " *** Do NOT use 3dFWHMx on the statistical results (e.g., '-bucket') from ***\n"
      " *** 3dDeconvolve or 3dREMLfit!!!  The function of 3dFWHMx is to estimate ***\n"
      " *** the smoothness of the time series NOISE, not of the statistics. This ***\n"
      " *** proscription is especially true if you plan to use 3dClustSim next!! ***\n"
      "\n"
      " *** -------------------                                                  ***\n"
      " *** NOTE FOR SPM USERS:                                                  ***\n"
      " *** -------------------                                                  ***\n"
      " *** If you are using SPM for your analyses, and wish to use 3dFHWMX plus ***\n"
      " *** 3dClustSim for cluster-level thresholds, you need to understand the  ***\n"
      " *** process that AFNI uses. Otherwise, you will likely make some simple  ***\n"
      " *** mistake (such as using 3dFWHMx on the statistical maps from SPM)     ***\n"
      " *** that will render your cluster-level thresholding completely wrong!   ***\n"
#if 1
      "\n"
      ">>>>>\n"
      "IF YOUR DATA HAS SMOOTH-ISH SPATIAL STRUCTURE YOU CAN'T GET RID OF:\n"
      "For example, you only have 1 volume, say from PET imaging.  In this case,\n"
      "the standard estimate of the noise smoothness will be mixed in with the\n"
      "structure of the background.  An approximate way to avoid this problem\n"
      "is provided with the semi-secret '-2difMAD' option, which uses a combination of\n"
      "first-neighbor and second-neighbor differences to estimate the smoothness,\n"
      "rather than just first-neighbor differences, and uses the MAD of the differences\n"
      "rather than the standard deviation.  (If you must know the details, read the\n"
      "source code in mri_fwhm.c!)                    [For Jatin Vaidya, March 2010]\n"
#endif
#if 0
      "\n"
      "IF YOU WISH TO ALLOW FOR SPATIAL VARIABILITY IN NOISE SMOOTHNESS:\n"
      "The semi-secret '-1difMOM' option uses moments of the first differences to\n"
      "crudely estimate a single smoothness that is intended to represent the effect\n"
      "of variable smoothness -- the idea being that larger smoothness has a bigger\n"
      "effect on the 3dClustSim results than smaller smoothness does, so the results\n"
      "from this option will tend to be larger than from the standard option.\n"
      "** This option is intended for use with single subject data; it is probably\n"
      "   too conservative to use this option on group data when the smoothness\n"
      "   is adjusted upwards based on single subject noise statistics.  [August 2015]\n"
      "** The adjustment upwards involves shifting an intermediate estimate by\n"
      "   a fraction of its standard deviation.  The default shift is 1.0 times the\n"
      "   standard deviation estimate.  If you want to see the result without this\n"
      "   shift, use '-1difMOM 0.0' (smoothness values will be smaller).\n"
#endif
      "\n"
      "ALSO SEE:\n"
      "* The older program 3dFWHM is now completely superseded by 3dFWHMx.\n"
      "* The program 3dClustSim takes as input the ACF estimates and then\n"
      "  estimates the cluster sizes thresholds to help you get 'corrected'\n"
      "  (for multiple comparisons) p-values.\n"
#if 1
      ">>>>>\n"
      "* 3dLocalstat -stat FWHM will estimate the FWHM values at each voxel,\n"
      "  using the same first-difference algorithm as this program, but applied\n"
      "  only to a local neighborhood of each voxel in turn.\n"
#endif
      "* 3dLocalACF will estimate the 3 ACF parameters in a local neighborhood\n"
      "  around each voxel.\n"
      ">>>>>\n"
      "* 3dBlurToFWHM will iteratively blur a dataset (inside a mask) to have\n"
      "  a given global FWHM. This program may or may not be useful :)\n"
      "* 3dBlurInMask will blur a dataset inside a mask, but doesn't measure FWHM or ACF.\n"
      "\n"
      "-- Zhark, Ruler of the (Galactic) Cluster!\n"
     ) ;
     PRINT_AFNI_OMP_USAGE("3dFWHMx",NULL) ; PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dFWHMx"); mainENTRY("3dFWHMx main"); machdep(); AUTHOR("The Bob");
   AFNI_logger("3dFWHMx",argc,argv) ;

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-detrend") == 0 ){          /* 10 May 2007 */
       corder = -1 ;
       if( iarg < argc-1 && isdigit(argv[iarg+1][0]) ){
         corder = (int)strtod(argv[++iarg],NULL) ;
         if( corder == 0 ){
           demed = 1 ; INFO_message("-detrend 0 replaced by -demed") ;
         }
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-2dif") == 0 ){             /* 20 Nov 2006 */
       mri_fwhm_setfester( mri_estimate_FWHM_12dif ) ;  /* secret option */
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-2difMAD") == 0 ){              /* 24 Mar 2010 */
       mri_fwhm_setfester( mri_estimate_FWHM_12dif_MAD ) ;  /* secret option */
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-1difMOM") == 0 ){              /* 11 Aug 2015 */
       mri_fwhm_setfester( mri_FWHM_1dif_mom12 ) ;
       iarg++ ;
       if( iarg < argc && argv[iarg][0] >= '0' && argv[iarg][0] <= '9' ){
         double val = strtod(argv[iarg++],NULL) ;
         if( val >= 0.0 && val <= 1.0 ) mri_fwhm_mom12_set_stdev_fac(val) ;
       }
       continue ;
     }

     if( strncmp(argv[iarg],"-geom",4) == 0 ){          /* 15 Nov 2006 */
       geom = 1 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-arith",5) == 0 ){         /* 15 Nov 2006 */
       geom = 0 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-demed",5) == 0 ){         /* 15 Nov 2006 */
       demed = 1 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-unif",5) == 0 ){          /* 07 Dec 2006 */
       unif = demed = 1 ; iarg++ ; continue ;
     }
     if( strncmp(argv[iarg],"-comb",4) == 0 ){          /* 24 Mar 2010 */
       combine = 1 ; iarg++ ; continue ;
     }

     if( strncmp(argv[iarg],"-compat",6) == 0 ){        /* 09 Nov 2006 */
       FHWM_1dif_dontcheckplus(1) ; iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-ACF",4) == 0 ){       /* 09 Nov 2015 */
       do_acf = 1 ; if( argv[iarg][1] == 'a' ) do_acf = -1 ;
       iarg++ ;
       if( iarg < argc && argv[iarg][0] != '-' ){
         acf_fname = strdup(argv[iarg]) ;
         if( !THD_filename_ok(acf_fname) )
           ERROR_exit("filename after -ACF is invalid!") ;
         iarg++ ;
         if( iarg < argc && isdigit(argv[iarg][0]) ){   /* 07 Dec 2015 */
           acf_rad = (float)strtod(argv[iarg],NULL) ;
           iarg++ ;
         }
       }
       continue ;
     }

     if( strcasecmp(argv[iarg],"-addcol5") == 0 ){
       addcol5++ ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-ShowMeClassicFWHM") == 0 ){  /* 20 Jul 2017 */
       do_classic = 1 ; iarg++ ;
       WARNING_message("Using the 'Classic' Gaussian FWHM is not recommended :(") ;
       ININFO_message (" The '-acf' method gives a FWHM estimate which is more robust;") ;
       ININFO_message (" however, assuming the spatial correlation of FMRI noise has") ;
       ININFO_message (" a Gaussian shape is not a good model.") ;
       continue ;
     }

     if( strncmp(argv[iarg],"-out",4) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-out'") ;
       outfile = argv[iarg] ;
            if( strcasecmp(outfile,"NULL") == 0 ) outfile = NULL ;
       else if( !THD_filename_ok(outfile) ) ERROR_exit("Illegal filename after '-out'") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-input") == 0 || strcmp(argv[iarg],"-dset") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 16 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-detprefix") == 0 ){
       newprefix = argv[++iarg] ; iarg++ ; continue ;
     }

     ERROR_exit("Uknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*-------------- deal with input dataset --------------*/

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }

   inset_prefix = strdup( DSET_PREFIX(inset) ) ;
   cpp = strstr(inset_prefix,".nii")   ; if( cpp != NULL ) *cpp = '\0' ;
   cpp = strstr(inset_prefix,"+orig")  ; if( cpp != NULL ) *cpp = '\0' ;
   cpp = strstr(inset_prefix,"+tlrc")  ; if( cpp != NULL ) *cpp = '\0' ;
   cpp = THD_trailname(inset_prefix,0) ; if( cpp != NULL ) inset_prefix = cpp ;

   { THD_3dim_dataset *qset = THD_remove_allzero(inset) ;  /* 25 Jul 2017 */
     if( qset != NULL ){
       DSET_delete(inset) ; inset = qset ;
     }
   }

   if( (demed || unif || corder ) && DSET_NVALS(inset) < 4 ){
     WARNING_message(
       "-demed and/or -corder and/or -unif ignored: only %d input sub-bricks",
       DSET_NVALS(inset) ) ;
     demed = corder = unif = 0 ;
   }

   if( demed && corder ){
     demed = 0 ; WARNING_message("-demed is overriden by -corder") ;
   }

   if( corder < 0 ){
     corder = DSET_NVALS(inset) / 30 ;
     if( corder == 0 ){
       WARNING_message("Fewer than 30 time points ==> -corder converted to -unif") ;
       unif = demed = 1 ;
     }
   } else if( corder > 0 && 2*corder+3 >= DSET_NVALS(inset) ){
     ERROR_exit("-corder %d is too big for this dataset",corder) ;
   }

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   nvals = DSET_NVALS(inset) ; nvox = DSET_NVOX(inset) ;

   if( mask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )
       ERROR_exit("-mask dataset grid dimensions don't match input dataset") ;

   } else if( automask ){
     int mmm ;
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     mmm = THD_countmask( DSET_NVOX(inset) , mask ) ;
     INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 16 ) ERROR_exit("Automask is too small to process") ;
   } else {
     mask = (byte *)malloc(sizeof(byte)*nvox) ;
     memset(mask,1,sizeof(byte)*nvox) ;
   }
   if( nvals > 3 ){
     for( ncon=ii=0 ; ii < nvox ; ii++ ){
       if( mask[ii] && THD_voxel_is_constant(ii,inset) ){ mask[ii] = 0; ncon++; }
     }
     if( ncon > 0 )
       WARNING_message("removed %d voxels from mask because they are constant in time",ncon) ;
   }

   /*-- if NOT detrending or de-median-ing, check if that's a good idea --*/

   if( !(corder > 0 || demed) && nvals > 4 ){  /* 13 Aug 2010 */
     MRI_IMARR *imar ;
     imar = THD_medmad_bricks(inset) ;
     if( imar != NULL ){
       float *med , *mad ; int nchk=0,nbad=0 ;
       med = MRI_FLOAT_PTR(IMARR_SUBIM(imar,0)) ;
       mad = MRI_FLOAT_PTR(IMARR_SUBIM(imar,1)) ;
       for( ii=0 ; ii < nvox ; ii++ ){
         if( mask[ii] && mad[ii] > 0.0f ){
           nchk++ ; if( fabsf(med[ii]) > 6.66f*mad[ii] ) nbad++ ;
         }
       }
       DESTROY_IMARR(imar) ;
       if( nbad > nchk/16 ){
         WARNING_message("Suggestion: use the '-detrend' option:") ;
         ININFO_message ("%d (out of %d) voxel time series have significant means",nbad,nchk) ;
       }
     }
   }

   /*-- if detrending, do that now --*/

   ct = COX_cpu_time() ;
   if( corder > 0 ){
     int nref=2*corder+3 , jj,iv,kk ;
     float **ref , tm,fac,fq ;
     THD_3dim_dataset *newset ;

     INFO_message("detrending start: %d baseline funcs, %d time points",nref,nvals) ;

     ref = THD_build_trigref( corder , nvals ) ;
     if( ref == NULL ) ERROR_exit("THD_build_trigref failed!") ;

     newset = THD_detrend_dataset( inset , nref , ref , 2 , 1 , mask , NULL ) ;
     if( newset == NULL ) ERROR_exit("detrending failed!") ;

     for(jj=0;jj<nref;jj++) free(ref[jj]) ;
     free(ref); DSET_delete(inset); inset=newset;
     demed = 0 ; unif = 1 ;
     ININFO_message("detrending done (%.2f CPU s thus far)",COX_cpu_time()-ct) ;

     if( newprefix != NULL ){    /** for debugging **/
       EDIT_dset_items(newset,ADN_prefix,newprefix,NULL) ;
       (void)THD_deconflict_prefix(newset) ;
       DSET_write(newset) ; WROTE_DSET(newset) ;
     }
   }

   /*-- do the FWHM-izing work --*/

   if( do_classic )
     INFO_message("start Classic FWHM calculations") ;

   outim = THD_estimate_FWHM_all( inset , mask , demed,unif ) ;

#if 0
   if( AFNI_yesenv("MOMENTS") )
     THD_estimate_FWHM_moments_all( inset , mask , demed,unif ) ;
#endif

   if( !do_acf ) DSET_unload(inset) ;

   if( outim == NULL ) ERROR_exit("Function THD_estimate_FWHM_all() fails?!") ;

   if( outfile != NULL ) mri_write_ascii( outfile , outim ) ;

   outar = MRI_FLOAT_PTR(outim) ;

   nx = thd_floatscan( 3*nvals, outar ) ;  /* 07 Dec 2006 */
   if( nx > 0 ) WARNING_message("found %d non-finite FWHM array values!",nx);

   nx = ny = nz = 0 ;
   if( geom ){
     cx = cy = cz = 0.0 ;
     for( ii=0 ; ii < nvals ; ii++ ){
       fx = outar[0+3*ii]; fy = outar[1+3*ii]; fz = outar[2+3*ii];
/* INFO_message("geom: fx=%g fy=%g fz=%g",fx,fy,fz) ; */
       if( fx > 0.0 ){ cx += log(fx) ; nx++ ; }
       if( fy > 0.0 ){ cy += log(fy) ; ny++ ; }
       if( fz > 0.0 ){ cz += log(fz) ; nz++ ; }
     }
     cx = (nx == 0) ? 0.0 : exp(cx/nx) ;
     cy = (ny == 0) ? 0.0 : exp(cy/ny) ;
     cz = (nz == 0) ? 0.0 : exp(cz/nz) ;
     ccomb = 1.0 ; ncomb = 0 ;
     if( cx > 0.0 ){ ccomb *= cx ; ncomb++ ; }
     if( cy > 0.0 ){ ccomb *= cy ; ncomb++ ; }
     if( cz > 0.0 ){ ccomb *= cz ; ncomb++ ; }
          if( ncomb == 2 ) ccomb = sqrt(ccomb) ;
     else if( ncomb == 3 ) ccomb = cbrt(ccomb) ;
   } else {
     cx = cy = cz = 0.0 ;
     for( ii=0 ; ii < nvals ; ii++ ){
       fx = outar[0+3*ii]; fy = outar[1+3*ii]; fz = outar[2+3*ii];
       if( fx > 0.0 ){ cx += fx ; nx++ ; }
       if( fy > 0.0 ){ cy += fy ; ny++ ; }
       if( fz > 0.0 ){ cz += fz ; nz++ ; }
     }
     cx = (nx == 0) ? 0.0 : cx/nx ;
     cy = (ny == 0) ? 0.0 : cy/ny ;
     cz = (nz == 0) ? 0.0 : cz/nz ;
     /* fix arithmetic mean    19 Jul 2010 [rickr] */
     ccomb = 0.0 ; ncomb = 0 ;
     if( cx > 0.0 ){ ccomb += cx ; ncomb++ ; }
     if( cy > 0.0 ){ ccomb += cy ; ncomb++ ; }
     if( cz > 0.0 ){ ccomb += cz ; ncomb++ ; }
     if( ncomb > 1 ) ccomb /= ncomb ;
   }

   if( do_classic )
     ININFO_message("Classic FWHM done (%.2f CPU s thus far)",COX_cpu_time()-ct) ;

   if( do_acf ){
     MCW_cluster *acf ; int pp ; float dx,dy,dz,arr ;

     if( acf_rad <= 0.0f ) acf_rad = 2.999f * ccomb ;
     dx = fabsf(DSET_DX(inset)); dy = fabsf(DSET_DY(inset)); dz = fabsf(DSET_DZ(inset));
     arr = 3.999f * cbrtf(dx*dy*dz) ; if( acf_rad < arr ) acf_rad = arr ;
     INFO_message("start ACF calculations out to radius = %.2f mm",acf_rad) ;

     acf = THD_estimate_ACF( inset , mask , demed,unif , acf_rad ) ;
     if( acf == NULL ) ERROR_exit("Error calculating ACF :-(") ;
#if 0
     printf("# ACF dx=%g dy=%g dz=%g\n", dx,dy,dz ) ;
     printf("dx  dy  dz  ACF\n") ;
     printf("--- --- --- ------\n") ;
     for( pp=0 ; pp < acf->num_pt ; pp++ )
       printf("%3d %3d %3d %.5f\n",
              acf->i[pp] , acf->j[pp] , acf->k[pp] , acf->mag[pp] ) ;
#endif

     acf_Epar = ACF_cluster_to_modelE( acf, fabsf(DSET_DX(inset)) ,
                                            fabsf(DSET_DY(inset)) ,
                                            fabsf(DSET_DZ(inset)) ) ;

     if( acf_fname != NULL && strcmp(acf_fname,"NULL") != 0 ) acf_im = ACF_get_1D() ;

     ININFO_message("ACF done (%.2f CPU s thus far)",COX_cpu_time()-ct) ;

     if( do_acf > 0 )
       printf("# old-style FWHM parameters\n") ;
     if( do_classic )
       printf(" %g  %g  %g     %g",cx,cy,cz,ccomb) ;
     else
       printf(" 0  0  0    0") ;
     printf("\n") ;

     if( do_acf > 0 )
       printf("# ACF model parameters for a*exp(-r*r/(2*b*b))+(1-a)*exp(-r/c) plus effective FWHM\n") ;
     printf(" %g  %g  %g    %g\n",acf_Epar.a,acf_Epar.b,acf_Epar.c,acf_Epar.d) ;

     if( acf_im != NULL ){
       char cmd[4096] ;

       if( addcol5 ){
         MRI_IMAGE *qim,*pim ; float *rar, *qar, sig ; MRI_IMARR *imar ;
         qim = mri_new( acf_im->nx , 1 , MRI_float ) ;
         qar = MRI_FLOAT_PTR(qim) ; rar = MRI_FLOAT_PTR(acf_im) ;
         sig = FWHM_TO_SIGMA(ccomb) ;
         for( pp=0 ; pp < acf_im->nx ; pp++ )
           qar[pp] = exp(-0.5*rar[pp]*rar[pp]/(sig*sig)) ;
         INIT_IMARR(imar) ; ADDTO_IMARR(imar,acf_im) ; ADDTO_IMARR(imar,qim) ;
         pim = mri_catvol_1D(imar,2) ; DESTROY_IMARR(imar) ; acf_im = pim ;
       }

       mri_write_1D( acf_fname , acf_im ) ;

      if( addcol5 ){
       INFO_message("ACF 1D file [radius ACF mixed_model gaussian_NEWmodel gaussian_OLDmodel] written to %s",acf_fname) ;
       sprintf(cmd,
         "1dplot -one -xlabel 'r (mm)'"
         " -ylabel 'Autocorrelation \\small [FWHM=\\green %.2f \\blue %.2f\\black]'"
         " -yaxis 0:1:10:2 -DAFNI_1DPLOT_BOXSIZE=0.004"
         " -plabel '\\small\\noesc %s\\esc\\red  %.2f*exp[-r^2/2*%.2f^2]+%.2f*exp[-r/%.2f]'"
         " -box -png %s.png -x %s'[0]' %s'[1]' %s'[2]' %s'[3]' %s'[4]'" ,
         acf_Epar.d , ccomb ,
         inset_prefix ,
         acf_Epar.a , acf_Epar.b , 1.0f-acf_Epar.a , acf_Epar.c ,
         acf_fname, acf_fname, acf_fname, acf_fname, acf_fname, acf_fname ) ;
      } else {
       INFO_message("ACF 1D file [radius ACF mixed_model gaussian_NEWmodel] written to %s",acf_fname) ;
       sprintf(cmd,
         "1dplot -one -xlabel 'r (mm)'"
         " -ylabel 'Autocorrelation \\small [FWHM=\\green %.2f\\black]'"
         " -yaxis 0:1:10:2 -DAFNI_1DPLOT_BOXSIZE=0.004"
         " -plabel '\\small\\noesc %s\\esc\\red  %.2f*exp[-r^2/2*%.2f^2]+%.2f*exp[-r/%.2f]'"
         " -box -png %s.png -x %s'[0]' %s'[1]' %s'[2]' %s'[3]'" ,
         acf_Epar.d ,
         inset_prefix ,
         acf_Epar.a , acf_Epar.b , 1.0f-acf_Epar.a , acf_Epar.c ,
         acf_fname, acf_fname, acf_fname, acf_fname, acf_fname ) ;
      }

       system(cmd) ;
       ININFO_message("and 1dplot-ed to file %s.png",acf_fname) ;
     }

   } else if( do_classic ){  /* no ACF -- the OLD way */

     printf(" %g  %g  %g",cx,cy,cz) ;
     if( combine ) printf("     %g",ccomb) ;
     printf("\n") ;
   }
   exit(0) ;
}
