#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset1,*dset2=NULL, *oset ;
   MRI_IMAGE *dbr1,*dbr2,*dbr3 ;
   char *prefix = "DFT" ;
   float   *mag, *real;
   complex *comp_array;
   int iarg=1 , doabs=0, ii, jj, kk, ll, nvox, nvals=1, isfloat=0;
   int nx, ny, nz, nfft=0 , detrend=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dDFT [-prefix ppp] [-abs] [-nfft N] [-detrend] dataset\n"
            "   where dataset is complex or float valued.\n"
            "\n"
            " -abs     == output float dataset = abs(DFT)\n"
            " -nfft N  == use 'N' for DFT length (must be >= #time points)\n"
            " -detrend == least-squares remove linear drift before DFT\n"
           ) ;
     exit(0) ;
   }

   mainENTRY("3dDFT main"); machdep(); AFNI_logger("3dDFT",argc,argv);
   AUTHOR("Kevin Murphy & Zhark the Glorious") ;
#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   /*-- options --*/

#define GOOD_TYPE(tt) ((tt)==MRI_complex || (tt)==MRI_float )

   while( iarg < argc && argv[iarg][0] == '-' ){

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

   if(DSET_BRICK_TYPE(dset1,0) == MRI_float) { isfloat = 1; }

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
   if( nfft <= 2 )
     INFO_message("Data length = %d ; FFT length = %d",nvals,ii) ;
   else if( ii > nfft )
     WARNING_message("Data length = %d ; replacing -nfft=%d with %d",
                     nvals,nfft,ii);
   nfft = ii ;

   /* make output dataset */

   oset = EDIT_empty_copy( dset1 ) ;
   EDIT_dset_items( oset ,
                      ADN_prefix , prefix ,
                      ADN_datum_all, (isfloat) ? MRI_float : MRI_complex  ,
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

   if( THD_is_file( DSET_HEADNAME(oset) ) )
     ERROR_exit("Output file %s exists -- will not overwrite!",
                 DSET_HEADNAME(oset) ) ;

   /* create empty bricks for output dataset */

   for( ii=0 ; ii < nfft ; ii++ )
     EDIT_substitute_brick( oset , ii ,
                            (doabs) ? MRI_float : MRI_complex ,
                            NULL ) ;

   /* Loop through timeseries and do DFT */

   comp_array = (complex *) calloc( sizeof(complex) , nfft);
   mag        = (float *)   calloc( sizeof(float)   , nfft);
   if (isfloat) { real = (float *)   calloc( sizeof(float)   , nfft); }

   for( ii=0 ; ii < nvox ; ii++ ){  /* loop over voxels */

     if(!isfloat){
       (void)THD_extract_array( ii , dset1 , 1 , comp_array ) ;
       if( detrend ) THD_linear_detrend_complex( nvals , comp_array ) ;
     } else {
       (void)THD_extract_array( ii , dset1 , 1 , real );
       if( detrend ) THD_linear_detrend( nvals , real , NULL,NULL ) ;
       for( jj=0 ; jj < nvals ; jj++ ) {
         comp_array[jj].r = real[jj]; comp_array[jj].i = 0.0f ;
       }
     }

     for( jj=nvals ; jj < nfft ; jj++ )
       comp_array[jj].r = comp_array[jj].i = 0.0f ;  /* zero pad */

    /* Perform the DFT at last! */

     csfft_cox( -1 , nfft, comp_array ) ;

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

   INFO_message("output dataset: %s\n",DSET_BRIKNAME(oset)) ;
   DSET_write( oset ) ;
   exit(0) ;
}
