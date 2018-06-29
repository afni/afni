#include "mrilib.h"

static char prefix[THD_MAX_PREFIX] = "pgram" ;

static float taper = 0.1f ;
static int   gfft  = 0 ;

static void PGRAM_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbrik, float *val ) ;

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *old_dset , *new_dset ;  /* input and output datasets */
   int nopt, nvals , nbin ;

   /*----- Help the pitiful user? -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
        "Usage: 3dPeriodogram [options] dataset\n"
        "Computes the periodogram of each voxel time series.\n"
        "(Squared FFT = a crude estimate of the power spectrum)\n"
        "\n"
        "--------\n"
        "Options:\n"
        "--------\n"
        " -prefix p = use string 'p' for the prefix of the\n"
        "               output dataset [DEFAULT = 'pgram']\n"
        " -taper    = fraction of data to taper [DEFAULT = 0.1]\n"
        " -nfft L   = set FFT length to 'L' points\n"
        "               (longer than the data ==> zero padding)\n"
        "               (shorter than the data ==> data pruning)\n"
        "------\n"
        "Notes:\n"
        "------\n"
        "* Output is in float format; number of sub-bricks will be\n"
        "   half the FFT length; sub-brick #0 = FFT bin #1, etc.\n"
        "* Grid spacing in the frequency (sub-brick) dimension will\n"
        "   be 1/(nfft*TR) where nfft=FFT length, TR=dataset timestep.\n"
        "* There is no '-mask' option.  The hyper-clever user could\n"
        "   use something like\n"
        "     '3dcalc( -a dset+orig -b mask+orig -expr a*b )'\n"
        "   to apply a binary mask on the command line.\n"
        "* Data is not scaled exactly as in the AFNI Power plugin.\n"
        "* Each time series is linearly detrended prior to FFT-ization.\n"
        "* FFT length defaults to be the next legal length >= input dataset.\n"
#if 0
        "* The program can only do FFT lengths that are factorable\n"
        "   into a product of powers of 2, 3, and 5, and are even.\n"
        "  ++ The largest power of 3 that is allowed is 3^3 = 27.\n"
        "  ++ The largest power of 5 that is allowed is 5^3 = 125.\n"
        "  ++ e.g., FFT of length 3*5*8=120 is possible.\n"
        "  ++ e.g., FFT of length 4*31 =124 is not possible.\n"
#else
        "* The program can only do FFT lengths that are positive even integers.\n"
#endif
        "  ++ '-nfft' with an illegal value will cause the program to fail.\n"
        "* If you want to do smaller FFTs, then average the periodograms\n"
        "   (to reduce random fluctuations), you can use 3dPeriodogram in\n"
        "   a script with \"[...]\" sub-brick selectors, then average\n"
        "   the results with 3dMean.\n"
        "* Or you could use the full-length FFT, then smooth that FFT\n"
        "   in the frequency direction (e.g., with 3dTsmooth).\n"
        "* This is a really quick hack for DH and PB and SfN.\n"
        "\n"
        "* Author = RWCox -- who doesn't want any bribe at all for this!\n"
        "                 -- http://ethics.od.nih.gov/topics/gifts.htm\n"
      ) ;
      printf(
        "\n"
        "---------------------------------------------------\n"
        "More Details About What 3dPeriodogram Actually Does\n"
        "---------------------------------------------------\n"
        "* Tapering is done with the Hamming window (if taper > 0):\n"
        "    Define npts   = number of time points analyzed (<= nfft)\n"
        "                    (i.e., the length of the input dataset)\n"
        "           ntaper = taper * npts / 2        (0 < taper <= 1)\n"
        "                  = number of points to taper on each end\n"
        "           ktop   = npts - ntaper\n"
        "           phi    = PI / ntaper\n"
        "    Then the k-th point (k=0..nfft-1) is tapered by\n"
        "      w(k) = 0.54 - 0.46 * cos(k*phi)           0    <= k < ntaper\n"
        "      w(k) = 0.54 + 0.46 * cos((k-ktop+1)*phi)  ktop <= k < npts\n"
        "      w(k) = 1.0                                otherwise\n"
        "    Also define P = sum{ w(k)*w(k) } from k=0..npts-1\n"
        "    (if ntaper = 0, then P = npts).\n"
        "\n"
        "* The result is the squared magnitude of the FFT of w(k)*data(k),\n"
        "  divided by P.  This division makes the result be the 'power',\n"
        "  which is to say the data's sum-of-squares ('energy') per unit\n"
        "  time (in units of 1/TR, not 1/sec) ascribed to each FFT bin.\n"
        "\n"
        "* Normalizing by P also means that the values output for different\n"
        "  amounts of tapering or different lengths of data are comparable.\n"
        "\n"
        "* To be as clear as I can: this program does NOT do any averaging\n"
        "  across multiple windows of the data (such as Welch's method does)\n"
        "  to estimate the power spectrum.  This program:\n"
        "  ++ tapers the data,\n"
        "  ++ zero-pads it to the FFT length,\n"
        "  ++ FFTs it (in time),\n"
        "  ++ squares it and divides by the P factor.\n"
        "\n"
        "* The number of output sub-bricks is nfft/2:\n"
        "     sub-brick #0 = FFT bin #1 = frequency 1/(nfft*dt)\n"
        "               #1 = FFT bin #2 = frequency 2/(nfft*dt)\n"
        "  et cetera, et cetera, et cetera.\n"
        "\n"
        "* If you desire to implement Welch's method for spectrum estimation\n"
        "  using 3dPeriodogram, you will have to run the program multiple\n"
        "  times, using different subsets of the input data, then average\n"
        "  the results with 3dMean.\n"
        "  ++ http://en.wikipedia.org/wiki/Welch's_method\n"
      ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* bureaucracy */

   mainENTRY("3dPeriodogram main"); machdep(); AFNI_logger("3dPeriodogram",argc,argv);
   PRINT_VERSION("3dPeriodogram"); AUTHOR("RW Cox");

   /*--- scan command line for options ---*/

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcasecmp(argv[nopt],"-nfft") == 0 ){
         int mmm ;
         if( ++nopt >= argc ) ERROR_exit("%s needs an argument!",argv[nopt-1]);
         gfft = (int)strtod(argv[nopt],NULL) ;
         if( gfft > 0 && gfft != (mmm=csfft_nextup_even(gfft)) )
           ERROR_exit(
            "%d is not a legal FFT length here: next largest legal value = %d" ,
            gfft , mmm ) ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("-prefix needs an argument!");
         MCW_strncpy(prefix,argv[nopt],THD_MAX_PREFIX) ;
         if( !THD_filename_ok(prefix) )
           ERROR_exit("%s is not a valid prefix!",prefix);
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-taper") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("-taper needs an argument!");
         taper = (float)strtod(argv[nopt],NULL) ;
         if( taper < 0.0f || taper > 1.0f ) ERROR_exit("-taper value out of range 0..1");
         nopt++ ; continue ;
      }

      /*-- Quien sabe'? --*/

      ERROR_exit("Unknown option: %s",argv[nopt]) ;
   }

   /*----- read input dataset -----*/

   if( nopt >= argc ) ERROR_exit("No input dataset!?") ;

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(old_dset) )
     ERROR_exit("Can't open dataset %s",argv[nopt]);
   DSET_load(old_dset) ;
   if( !DSET_LOADED(old_dset) )
     ERROR_exit("Can't load dataset %s",argv[nopt]) ;

   nvals = DSET_NVALS(old_dset) ;
   if( nvals < 9 )
      ERROR_exit("Number of time points = %d < min value of 9!",nvals) ;

   nopt++ ;
   if( nopt < argc )
     WARNING_message("Trailing inputs on command line ignored: %s ...",argv[nopt]) ;

   if( gfft <= 0 ) gfft = csfft_nextup_even(nvals) ;
   nbin = gfft / 2 ;
   INFO_message("Dataset Lengths: Input=%d  FFT=%d  Output=%d" ,
                nvals, gfft, nbin ) ;

   /*------------- ready to compute new dataset -----------*/

   new_dset = MAKER_4D_to_typed_fbuc(
                    old_dset ,             /* input dataset */
                    prefix ,               /* output prefix */
                    MRI_float ,            /* output datum  */
                    0 ,                    /* ignore count  */
                    1 ,                    /* detrend (yes) */
                    nbin ,                 /* number of briks in output */
                    PGRAM_tsfunc ,         /* timeseries processor function */
                    NULL,                  /* extra data for tsfunc */
                    NULL,                  /* mask */
                    0                      /* Allow auto scaling of output */
                 ) ;

   if( new_dset != NULL ){
     float tr=DSET_TR(old_dset) , df ;

     if( DSET_TIMEUNITS(old_dset) == UNITS_MSEC_TYPE ) tr *= 0.001f ;

     if( tr <= 0.0f ) tr = 1.0f ;
     df = 1.0f / (gfft*tr) ;

     tross_Copy_History( old_dset , new_dset ) ;
     tross_Make_History( "3dPeriodogram" , argc,argv , new_dset ) ;
     EDIT_dset_items( new_dset ,
                         ADN_ntt    , nbin ,
                         ADN_ttorg  , df ,
                         ADN_ttdel  , df ,
                         ADN_tunits , UNITS_HZ_TYPE ,
                       NULL ) ;

     DSET_write( new_dset ) ; WROTE_DSET( new_dset ) ;
   } else {
     ERROR_exit("Unable to compute output dataset!?") ;
   }

   exit(0) ;
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void PGRAM_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void *ud, int nbrik, float *val )
{
   static float    *tar=NULL ;
   static complex *cxar=NULL ;
   static int      first=1 , nallz=0 , noutz=0 , nvox=0 ;
   static float    pfact=0.0f ;

   int nbin=nbrik , nfft=gfft ;

   register int kk ; int nz ;

ENTRY("PGRAM_tsfunc") ;

   /** is this a "notification"? **/

   if( val == NULL ){
      if( npts > 0 ){  /* the "start notification" */

      } else {  /* the "end notification" */

        if( tar  != NULL ){ free(tar) ; tar  = NULL; }
        if( cxar != NULL ){ free(cxar); cxar = NULL; }
        if( nallz > 0 )
          ININFO_message("%d/%d input voxel%s were all zero",nallz,nvox,((nallz==1)?"\0":"s")) ;
        if( noutz > 0 )
          ININFO_message("%d/%d output voxel%s were all zero",noutz,nvox,((noutz==1)?"\0":"s")) ;
        first = 1 ; nallz = noutz = nvox = 0 ; pfact = 0.0f ;

      }
      EXRETURN ;
   }

   if( npts > nfft ) npts = nfft ;  /* truncate data */

   if( first ){
     int ntaper , ktbot ; float phi ;

     cxar = (complex *)malloc(sizeof(complex)*nfft) ;
     tar  = (float *  )malloc(sizeof(float  )*nfft) ;
     ntaper = (int)(0.5f * taper * npts + 0.49f); /* will taper data over */
     phi    = PI / MAX(ntaper,1) ;                /* kk=0..ntaper-1 on left */
     ktbot  = npts - ntaper ;                     /* kk=ktbot..npts-1 on right */
     pfact  = 0.0f ;                              /* sum of taper**2 */

     for( kk=0 ; kk < npts ; kk++ ){                        /* Hamming-ize */
       if( kk < ntaper )
         tar[kk] = 0.54f - 0.46f * cosf(kk*phi) ;           /* ramp up */
       else if( kk >= ktbot )
         tar[kk] = 0.54f + 0.46f * cosf((kk-ktbot+1)*phi) ; /* ramp down */
       else
         tar[kk] = 1.0f ;                                   /* in the middle */

       pfact += tar[kk]*tar[kk] ;
     }
     pfact = 1.0f / pfact ;
     first = 0 ;
     ININFO_message("Taper length at each end of data = %d",ntaper) ;
   }

   /** do the work **/

   nvox++ ;

   for( kk=0 ; kk < npts && ts[kk] == 0.0f ; kk++ ) ; /*nada*/
   if( kk == npts ){
     for( kk=0 ; kk < nbin ; kk++ ) val[kk] = 0.0f ;  /* input was all zero */
     nallz++ ;
     EXRETURN ;
   }

   for( kk=0 ; kk < nfft ; kk++ ){
     if( kk < npts ) cxar[kk].r = tar[kk] * ts[kk] ;
     else            cxar[kk].r = 0.0f ;
     cxar[kk].i = 0.0f ;
   }

   csfft_cox( -1 , nfft , cxar ) ;

   for( nz=kk=0 ; kk < nbin ; kk++ ){
     val[kk] = CSQR(cxar[kk+1]) * pfact ;
     if( val[kk] == 0.0f ) nz++ ;
   }
   if( nz == nbin ) noutz++ ;

   EXRETURN ;
}
