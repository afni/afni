#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset1,*dset2=NULL, *oset ;
   MRI_IMAGE *dbr1,*dbr2,*dbr3 ;
   char *prefix = "DFT" ;
   float   *mag, *real=NULL;
   complex *comp_array;
   int iarg=1 , doabs=0, ii, jj, kk, ll, nvox, nvals=1, isfloat=0;
   int nx, ny, nz, nfft=0 , detrend=0 , sgn=-1 ;
   float *xtap=NULL , ftap=0.0f ;  /* 27 Nov 2007 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dDFT [-prefix ppp] [-abs] [-nfft N] [-detrend] dataset\n"
            "       where 'dataset' is complex- or float-valued.\n"
            " * Carries out the DFT along the time axis.\n"
            " * To do the DFT along the spatial axes, use program 3dFFT.\n"
            " * If you want to process short-valued datasets, you'll have\n"
            "   to convert to floating point format first, as in\n"
            "     3dcalc -a shortset+orig -prefix floatset -float -expr a\n"
            "\n"
            " -abs     == output float dataset = abs(DFT)\n"
            "            * Otherwise, the output file is complex-valued.\n"
            "              You can then use 3dcalc to extract the real part, the\n"
            "              imaginary part, the phase, etc.; see its '-cx2r' option:\n"
            "                3dcalc -a cxset+orig -cx2r REAL -expr a -prefix rset+orig\n"
            "            * Please note that if you view a complex dataset in AFNI,\n"
            "              the default operation is that you are looking at the\n"
            "              absolute value of the dataset.\n"
            "             ++ You can control the way a complex IMAGE appears via\n"
            "                the 'Disp' control panel (ABS, PHASE, REAL, IMAGE).\n"
            "             ++ You can control the way a complex TIME SERIES graph appears\n"
            "                via environment variable AFNI_GRAPH_CX2R (in 'EditEnv').\n"
            "\n"
            " -nfft N  == use 'N' for DFT length (must be >= #time points)\n"
            "\n"
            " -detrend == least-squares remove linear drift before DFT\n"
            "              [for more intricate detrending, use 3dDetrend first]\n"
            "\n"
            " -taper f == taper 'f' fraction of data at ends (0 <= f <= 1).\n"
            "              [Hamming 'raised cosine' taper of f/2 of the ]\n"
            "              [data length at each end; default is no taper]\n"
            "              [cf. 3dPeriodogam -help for tapering details!]\n"
            "\n"
            " -inverse == Do the inverse DFT:\n"
            "               SUM{ data[j] * exp(+2*PI*i*j/nfft) } * 1/nfft\n"
            "             instead of the forward transform\n"
            "               SUM{ data[j] * exp(-2*PI*i*j/nfft) }\n"
           ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dDFT main"); machdep(); AFNI_logger("3dDFT",argc,argv);
   AUTHOR("Kevin Murphy & Zhark the Transformer") ;
#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   /*-- options --*/

#define GOOD_TYPE(tt) ((tt)==MRI_complex || (tt)==MRI_float )

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-inverse") == 0 ){  /* 15 Apr 2011 */
        sgn = 1 ; csfft_scale_inverse(1) ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-taper") == 0 ){  /* 27 Nov 2007 */
        ftap = (float)strtod(argv[++iarg],NULL) ;
        if( ftap < 0.0f || ftap > 1.0f )
          ERROR_exit("Illegal value after -taper: %g",ftap) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) )
           ERROR_exit("-prefix %s is illegal!",prefix) ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-abs") == 0 ){
        doabs = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-detrend") == 0 ){
        detrend = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-nfft") == 0 ){
        nfft = (int)strtod(argv[++iarg],NULL) ;
        if( nfft <= 2 ){
          WARNING_message("Illegal -nfft value on command line") ;
          nfft = 0 ;
        } else {
          ii = csfft_nextup(nfft) ;
          if( ii > nfft ){
            WARNING_message("Replacing -nfft=%d with next largest legal value=%d",
                            nfft,ii) ;
            nfft = ii ;
          }
        }
        iarg++ ; continue ;
      }

      ERROR_exit("ILLEGAL option: %s\n",argv[iarg]) ;
   }

   if( iarg >= argc )
     ERROR_exit("No datasets on command line!?") ;

   /*-- read data --*/

#define OPENIT(ds,aa)                             \
 do{ ds = THD_open_dataset(aa) ;                  \
     if( !ISVALID_DSET(ds) )                      \
       ERROR_exit("Can't open dataset %s",aa);    \
     DSET_load(ds) ;                              \
     if( !DSET_LOADED(ds) )                       \
       ERROR_exit("Can't load dataset %s",aa);    \
 } while(0)

   OPENIT(dset1,argv[iarg])   ;
   if( !GOOD_TYPE( DSET_BRICK_TYPE(dset1,0) ) )
     ERROR_exit("ILLEGAL dataset type in %s - must be complex or float\n",argv[iarg]) ;

   isfloat = ( DSET_BRICK_TYPE(dset1,0) == MRI_float ) ;

   dbr1 = DSET_BRICK(dset1,0) ;

   nx = dbr1->nx;
   ny = dbr1->ny;
   nz = dbr1->nz;

   nvox = dbr1->nvox ;
   nvals = DSET_NVALS(dset1);

   /* Calculate size for FFT */
#if 0
   nfft = csfft_nextup_one35(nvals);
#endif

   ii = csfft_nextup(nvals);
   if( nfft <= 2 ){
     INFO_message("Data length = %d ; FFT length = %d",nvals,ii) ;
   } else if( ii > nfft ){
     WARNING_message("Data length = %d ; replacing -nfft=%d with %d",
                     nvals,nfft,ii);
     nfft = ii ;
   }

   if( ftap > 0.0f )
     xtap = mri_setup_taper( nvals , ftap ) ;  /* 27 Nov 2007 */

   /* make output dataset */

   oset = EDIT_empty_copy( dset1 ) ;
   EDIT_dset_items( oset ,
                      ADN_prefix , prefix ,
                      ADN_datum_all, (doabs) ? MRI_float : MRI_complex  ,
                      ADN_nvals  , nfft ,
                      ADN_ntt    , nfft ,
                    ADN_none ) ;

   DSET_UNMSEC(dset1) ;
   if( DSET_TIMEUNITS(dset1) == UNITS_SEC_TYPE ){   /* 'time' becomes Hz */
     float dt = DSET_TR(dset1) ;
     if( dt <= 0.0f ) dt = 1.0f ;
     EDIT_dset_items( oset ,
                        ADN_tunits , UNITS_HZ_TYPE ,
                        ADN_ttdel  , 1.0f/(nfft*dt) ,
                        ADN_nsl    , 0 ,
                      ADN_none ) ;
   }

#if 0
   if( THD_is_file( DSET_HEADNAME(oset) ) )
     ERROR_exit("Output file %s exists -- will not overwrite!",
                 DSET_HEADNAME(oset) ) ;
#else
   if( THD_deconflict_prefix(oset) > 0 )
     WARNING_message("Filename conflict: changing '%s' to '%s'",
                     prefix , DSET_PREFIX(oset) ) ;
#endif

   /* create empty bricks for output dataset */

   for( ii=0 ; ii < nfft ; ii++ )
     EDIT_substitute_brick( oset , ii ,
                            (doabs) ? MRI_float : MRI_complex ,
                            NULL ) ;

   /* Loop through timeseries and do DFT */

   comp_array = (complex *) calloc( sizeof(complex) , nfft);
   mag        = (float *)   calloc( sizeof(float)   , nfft);
   if( isfloat ) real = (float *)calloc( sizeof(float),nfft) ;

   for( ii=0 ; ii < nvox ; ii++ ){  /* loop over voxels */

     if( !isfloat ){
       (void)THD_extract_array( ii , dset1 , 1 , comp_array ) ;
       if( detrend ) THD_linear_detrend_complex( nvals , comp_array ) ;
     } else {
       (void)THD_extract_array( ii , dset1 , 1 , real );
       if( detrend ) THD_linear_detrend( nvals , real , NULL,NULL ) ;
       for( jj=0 ; jj < nvals ; jj++ ) {
         comp_array[jj].r = real[jj]; comp_array[jj].i = 0.0f ;
       }
     }

     if( xtap != NULL ){                 /* 27 Nov 2007 */
       for( jj=0 ; jj < nvals ; jj++ ){
         comp_array[jj].r *= xtap[jj] ; comp_array[jj].i *= xtap[jj] ;
       }
     }

     for( jj=nvals ; jj < nfft ; jj++ )
       comp_array[jj].r = comp_array[jj].i = 0.0f ;  /* zero pad */

    /**** Perform the DFT at last! ****/

     csfft_cox( sgn , nfft , comp_array ) ;

     if( doabs ){
       for( jj=0 ; jj < nfft ; jj++ ) mag[jj] = CABS(comp_array[jj]) ;
       THD_insert_series( ii , oset , nfft , MRI_float   , mag        , 1 );
     } else {
       THD_insert_series( ii , oset , nfft , MRI_complex , comp_array , 1 );
     }
   }

   DSET_unload(dset1) ;

   /* make history */

   tross_Copy_History( oset , dset1 ) ;
   tross_Make_History( "3dDFT", argc,argv, oset ) ;

   INFO_message("Output %s-valued dataset: %s\n",
                (doabs) ? "float" : "complex" , DSET_BRIKNAME(oset) ) ;
   DSET_write( oset ) ;
   exit(0) ;
}
