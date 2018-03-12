#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#ifdef USE_OMP
#include "thd_Tcorr1D.c"
#endif

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *xset=NULL , *cset ;
   int nopt=1, datum=MRI_float, nvals, ii;
   MRI_IMAGE *ysim=NULL ;
   char *prefix = "Tcorr1D", *smethod="pearson";
   char *xnam=NULL , *ynam=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , nmask=0 ;
   int do_atanh = 0 ; /* 12 Jan 2018 */

   /*----*/

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTcorr1D [options] xset y1D\n"
             "Computes the correlation coefficient between each voxel time series\n"
             "in the input 3D+time dataset 'xset' and each column in the 1D time\n"
             "series file 'y1D', and stores the output values in a new dataset.\n"
             "\n"
             "OPTIONS:\n"
             "  -pearson  = Correlation is the normal Pearson (product moment)\n"
             "                correlation coefficient [this is the default method].\n"
             "  -spearman = Correlation is the Spearman (rank) correlation\n"
             "                coefficient.\n"
             "  -quadrant = Correlation is the quadrant correlation coefficient.\n"
             "  -ktaub    = Correlation is Kendall's tau_b coefficient.\n"
             "              ++ For 'continuous' or finely-discretized data, tau_b and\n"
             "                 rank correlation are nearly equivalent (but not equal).\n"
             "  -dot      = Doesn't actually compute a correlation coefficient; just\n"
             "                calculates the dot product between the y1D vector(s)\n"
             "                and the dataset time series.\n"
             "\n"
             "  -Fisher   = Apply the 'Fisher' (inverse hyperbolic tangent) transformation\n"
             "                to the results.\n"
             "              ++ It does not make sense to use this with '-ktaub', but if\n"
             "                 you want to do it, the program will not stop you.\n"
             "              ++ Cannot be used with '-dot'!\n"
             "\n"
             "  -prefix p = Save output into dataset with prefix 'p'\n"
             "               [default prefix is 'Tcorr1D'].\n"
             "\n"
             "  -mask mmm = Only process voxels from 'xset' that are nonzero\n"
             "                in the 3D mask dataset 'mmm'.\n"
             "              ++ Other voxels in the output will be set to zero.\n"
             "\n"
             "  -float    = Save results in float format [the default format].\n"
             "  -short    = Save results in scaled short format [to save disk space].\n"
             "              ++ Cannot be used with '-dot'!\n"
             "\n"
             "NOTES:\n"
             "* The output dataset is functional bucket type, with one sub-brick\n"
             "   per column of the input y1D file.\n"
             "* No detrending, blurring, or other pre-processing options are available;\n"
             "   if you want these things, see 3dDetrend or 3dTproject or 3dcalc.\n"
             "   [In other words, this program presumes you know what you are doing!]\n"
             "* Also see 3dTcorrelate to do voxel-by-voxel correlation of TWO\n"
             "   3D+time datasets' time series, with similar options.\n"
             "* You can extract the time series from a single voxel with given\n"
             "   spatial indexes using 3dmaskave, and then run it with 3dTcorr1D:\n"
             "    3dmaskave -quiet -ibox 40 30 20 epi_r1+orig > r1_40_30_20.1D\n"
             "    3dTcorr1D -pearson -Fisher -prefix c_40_30_20 epi_r1+orig r1_40_30_20.1D\n"
             "* http://en.wikipedia.org/wiki/Correlation\n"
             "* http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient\n"
             "* http://en.wikipedia.org/wiki/Spearman%%27s_rank_correlation_coefficient\n"
             "* http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient\n"
             "\n"
             "-- RWCox - Apr 2010\n"
             "         - Jun 2010: Multiple y1D columns; OpenMP; -short; -mask.\n"
            ) ;
      PRINT_AFNI_OMP_USAGE("3dTcorr1D",NULL) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dTcorr1D main"); machdep(); AFNI_logger("3dTcorr1D",argc,argv);
   PRINT_VERSION("3dTcorr1D") ; THD_check_AFNI_version("3dTcorr1D") ;

   /*-- option processing --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcmp(argv[nopt],"-mask") == 0 ){  /* 28 Jun 2010 */
       THD_3dim_dataset *mset ;
       if( ++nopt >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL )   ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[nopt] ) ;
       CHECK_OPEN_ERROR(mset,argv[nopt]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[nopt]) ;
       nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",nmask) ;
       if( nmask < 2 ) ERROR_exit("Mask is too small to process") ;
       nopt++ ; continue ;
     }

      if( strcasecmp(argv[nopt],"-float") == 0 ){  /* 27 Jun 2010 */
        datum = MRI_float ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-short") == 0 ){
        datum = MRI_short ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-pearson") == 0 ){
        smethod = "pearson" ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-dot") == 0 ){
        smethod = "dot" ; nopt++ ; continue ;
      }


      if( strcasecmp(argv[nopt],"-spearman") == 0 || strcasecmp(argv[nopt],"-rank") == 0 ){
        smethod = "spearman" ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-quadrant") == 0 ){
        smethod = "quadrant" ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-ktaub") == 0 || strcasecmp(argv[nopt],"-taub") == 0 ){
        smethod = "ktaub" ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-fisher") == 0 ){ /* 12 Jan 2018 */
        do_atanh = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-prefix") == 0 ){
        prefix = argv[++nopt] ;
        if( !THD_filename_ok(prefix) ) ERROR_exit("Illegal value after -prefix!") ;
        nopt++ ; continue ;
      }

      ERROR_exit("Unknown option: %s",argv[nopt]) ;
   }

   /*------------ open datasets, check for legality ------------*/

   if( nopt+1 >= argc )
     ERROR_exit("Need 2 non-option arguments on command line!?") ;

   /* despite what the help says, if the 1D file is first, that's OK */

   if( STRING_HAS_SUFFIX(argv[nopt],"1D") ){
     ININFO_message("reading 1D file %s",argv[nopt]) ;
     ysim = mri_read_1D( argv[nopt] ) ; ynam = argv[nopt] ;
     if( ysim == NULL ) ERROR_exit("Can't read 1D file %s",argv[nopt]) ;
   } else {
     ININFO_message("reading dataset file %s",argv[nopt]) ;
     xset = THD_open_dataset( argv[nopt] ) ; xnam = argv[nopt] ;
     if( xset == NULL ) ERROR_exit("Can't open dataset %s",argv[nopt]) ;
   }

   /* read whatever type of file (3D or 1D) we don't already have */

   nopt++ ;
   if( xset != NULL ){
     ININFO_message("reading 1D file %s",argv[nopt]) ;
     ysim = mri_read_1D( argv[nopt] ) ; ynam = argv[nopt] ;
     if( ysim == NULL ) ERROR_exit("Can't read 1D file %s",argv[nopt]) ;
   } else {
     ININFO_message("reading dataset file %s",argv[nopt]) ;
     xset = THD_open_dataset( argv[nopt] ) ; xnam = argv[nopt] ;
     if( xset == NULL ) ERROR_exit("Can't open dataset %s",argv[nopt]) ;
   }

   nvals = DSET_NVALS(xset) ;  /* number of time points */
   ii    = (strcmp(smethod,"dot")==0) ? 2 : 3 ;
   if( nvals < ii )
     ERROR_exit("Input dataset %s length is less than ii?!",xnam,ii) ;

   if( ysim->nx < nvals )
     ERROR_exit("1D file %s has %d time points, but dataset has %d values",
                ynam,ysim->nx,nvals) ;
   else if( ysim->nx > nvals )
     WARNING_message("1D file %s has %d time points, dataset has %d",
                     ynam,ysim->nx,nvals) ;

   if( mri_allzero(ysim) )
     ERROR_exit("1D file %s is all zero!",ynam) ;

   if( ysim->ny > 1 )
     INFO_message("1D file %s has %d columns: correlating with ALL of them!",
                   ynam,ysim->ny) ;

   if( strcmp(smethod,"dot") == 0 && do_atanh ){
     WARNING_message("'-dot' turns off '-Fisher'") ; do_atanh = 0 ;
   }
   if( strcmp(smethod,"dot") == 0 && datum == MRI_short ){
     WARNING_message("'-dot' turns off '-short'") ; datum = MRI_float ;
   }

   cset = THD_Tcorr1D( xset, mask, nmask, ysim, smethod,
                       prefix, (datum==MRI_short) , do_atanh );
   tross_Make_History( "3dTcorr1D" , argc,argv , cset ) ;

   DSET_unload(xset) ;  /* no longer needful */

   /* finito */

   DSET_write(cset) ;
   INFO_message("Wrote dataset: %s\n",DSET_BRIKNAME(cset)) ;
   exit(0) ;
}
