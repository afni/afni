#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#define SPEARMAN 1
#define QUADRANT 2
#define PEARSON  3
#define KTAUB    4

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *xset=NULL , *cset ;
   int nopt=1 , method=PEARSON , do_autoclip=0 ;
   int nvox , nvals , ii ;
   MRI_IMAGE *ysim=NULL ;
   char *prefix = "Tcorr1D" ;
   char *xnam=NULL , *ynam=NULL ;
   int ny, kk, datum=MRI_float ; char str[32], fmt[32] ; float cfac=0.0f ;
   float (*corfun)(int,float *,float *) ;  /* ptr to correlation function */

   /*----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTcorr1D [options] xset y1D\n"
             "Computes the correlation coefficient between each voxel time series\n"
             "in the input 3D+time dataset 'xset' and the 1D time series file 'y1D',\n"
             "and stores the output values in a new 1 sub-brick dataset.\n"
             "\n"
             "OPTIONS:\n"
             "  -pearson  = Correlation is the normal Pearson (product moment)\n"
             "                correlation coefficient [this is the default method].\n"
             "  -spearman = Correlation is the Spearman (rank) correlation\n"
             "                coefficient.\n"
             "  -quadrant = Correlation is the quadrant correlation coefficient.\n"
             "  -ktaub    = Correlation is Kendall's tau_b coefficient.\n"
             "              ++ For 'continuous' or finely-discretized data, tau_b\n"
             "                 and rank correlation are nearly equivalent.\n"
             "\n"
             "  -prefix p = Save output into dataset with prefix 'p'\n"
             "               [default prefix is 'Tcorr1D'].\n"
             "\n"
             "  -float    = Save results in float format.\n"
             "  -short    = Save results in scaled short format.\n"
             "\n"
             "NOTES:\n"
             "* The output dataset is functional bucket type, with one sub-brick\n"
             "   per column of the input y1D file.\n"
             "* No detrending, mean-removal, masking, or other such options are available;\n"
             "   if you want these things, see 3dDetrend or 3dBandpass or 3dcalc.\n"
             "   [In other words, this program presumes you know what you are doing!]\n"
             "* Also see 3dTcorrelate to do voxel-by-voxel correlation of TWO\n"
             "   3D+time datasets' time series, with similar options.\n"
             "* http://en.wikipedia.org/wiki/Correlation\n"
             "* http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient\n"
             "* http://en.wikipedia.org/wiki/Spearman%%27s_rank_correlation_coefficient\n"
             "* http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient\n"
             "\n"
             "-- RWCox - Apr 2010\n"
             "         - Jun 2010: Multiple y1D columns; OpenMP; -short [for HJJ].\n"
            ) ;
      PRINT_AFNI_OMP_USAGE("3dTcorr1D",NULL) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dTcorr1D main"); machdep(); AFNI_logger("3dTcorr1D",argc,argv);
   PRINT_VERSION("3dTcorr1D") ;

   /*-- option processing --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcasecmp(argv[nopt],"-float") == 0 ){
        datum = MRI_float ; nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-short") == 0 ){
        datum = MRI_short ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-pearson") == 0 ){
        method = PEARSON ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-spearman") == 0 ){
        method = SPEARMAN ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-quadrant") == 0 ){
        method = QUADRANT ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-ktaub") == 0 ){
        method = KTAUB ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-prefix") == 0 ){
        prefix = argv[++nopt] ;
        if( !THD_filename_ok(prefix) )
          ERROR_exit("Illegal value after -prefix!") ;
        nopt++ ; continue ;
      }

      ERROR_exit("Unknown option: %s",argv[nopt]) ;
   }

   /*-- open datasets, check for legality --*/

   if( nopt+1 >= argc )
     ERROR_exit("Need 2 non-option arguments on command line!?") ;

   if( STRING_HAS_SUFFIX(argv[nopt],"1D") ){
     INFO_message("reading 1D file %s",argv[nopt]) ;
     ysim = mri_read_1D( argv[nopt] ) ; ynam = argv[nopt] ;
     if( ysim == NULL )
       ERROR_exit("Can't read 1D file %s",argv[nopt]) ;
   } else {
     INFO_message("reading dataset file %s",argv[nopt]) ;
     xset = THD_open_dataset( argv[nopt] ) ; xnam = argv[nopt] ;
     if( xset == NULL )
       ERROR_exit("Can't open dataset %s",argv[nopt]) ;
   }

   nopt++ ;
   if( xset != NULL ){
     INFO_message("reading 1D file %s",argv[nopt]) ;
     ysim = mri_read_1D( argv[nopt] ) ; ynam = argv[nopt] ;
     if( ysim == NULL )
       ERROR_exit("Can't read 1D file %s",argv[nopt]) ;
   } else {
     INFO_message("reading dataset file %s",argv[nopt]) ;
     xset = THD_open_dataset( argv[nopt] ) ; xnam = argv[nopt] ;
     if( xset == NULL )
       ERROR_exit("Can't open dataset %s",argv[nopt]) ;
   }

   nvals = DSET_NVALS(xset) ;
   if( nvals < 3 )
     ERROR_exit("Input dataset %s length is less than 3?!",xnam) ;

   if( ysim->nx < nvals )
     ERROR_exit("1D file %s has %d time points, but dataset has %d values",
                ynam,ysim->nx,nvals) ;
   else if( ysim->nx > nvals )
     WARNING_message("1D file %s has %d time points, dataset has %d",
                     ynam,ysim->nx,nvals) ;

   if( mri_allzero(ysim) )
     ERROR_exit("1D file %s is all zero!",ynam) ;

   ny = ysim->ny ;
   if( ny > 1 )
     WARNING_message("1D file %s has %d columns: correlating with ALL of them!",
                     ynam,ny) ;

   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   nvox = DSET_NVOX(xset) ;

   /*-- create output dataset --*/

   cset = EDIT_empty_copy( xset ) ;
   EDIT_dset_items( cset ,
                      ADN_prefix    , prefix         ,
                      ADN_nvals     , ny             ,
                      ADN_ntt       , 0              , /* no time axis */
                      ADN_type      , HEAD_FUNC_TYPE ,
                      ADN_func_type , FUNC_BUCK_TYPE ,
                    ADN_none ) ;

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(cset)) ){
     fprintf(stderr,"** Output dataset %s already exists!\n",
             DSET_HEADNAME(cset)) ;
     exit(1) ;
   }

        if( ny <   10 ) kk = 1 ;
   else if( ny <  100 ) kk = 2 ;
   else if( ny < 1000 ) kk = 3 ;
   else                 kk = 4 ;
   switch( method ){                                   /* looks nice  */
     default:
     case PEARSON:  sprintf(fmt,"PearCorr#%%%dd",kk) ; break ;
     case SPEARMAN: sprintf(fmt,"SpmnCorr#%%%dd",kk) ; break ;
     case QUADRANT: sprintf(fmt,"QuadCorr#%%%dd",kk) ; break ;
     case KTAUB:    sprintf(fmt,"TaubCorr#%%%dd",kk) ; break ;
   }
   if( datum == MRI_short ) cfac = 0.0001f ;

   for( kk=0 ; kk < ny ; kk++ ){
     EDIT_substitute_brick(cset,kk,datum,NULL) ; /* make array  */
     EDIT_BRICK_TO_FICO(cset,kk,nvals,1,1) ;    /* stat params */
     EDIT_BRICK_FACTOR(cset,kk,cfac) ;
     sprintf(str,fmt,kk) ;
     EDIT_BRICK_LABEL(cset,kk,str) ;            /* labelize */
   }

   tross_Make_History( "3dTcorr1D" , argc,argv , cset ) ;

   switch( method ){                    /* correlate away! */
     default:
     case PEARSON:  corfun = THD_pearson_corr  ; break ;
     case SPEARMAN: corfun = THD_spearman_corr ; break ;
     case QUADRANT: corfun = THD_quadrant_corr ; break ;
     case KTAUB:    corfun = THD_ktaub_corr    ; break ;
   }

   /* 27 Jun 2010: loop over columns in ysim */

#pragma omp parallel if( ny > 1 )
 { float *ysar, *xsar, *fcar, *ydar, val ; int ii, kk, jj ; short *scar ;
 AFNI_OMP_START ;

#ifdef USE_OMP
   if( omp_get_thread_num() == 0 )
     INFO_message("Start correlations: %d voxels X %d time series; %d threads",
                  nvox , ny , omp_get_num_threads() ) ;
#else
   INFO_message("Start correlations: %d voxels X %d time series") ;
#endif

   ydar = (float *)malloc(sizeof(float)*nvals) ;  /* 1D data duplicate */
   xsar = (float *)malloc(sizeof(float)*nvals) ;  /* 3D data duplicate */

#pragma omp for
   for( kk=0 ; kk < ny ; kk++ ){  /* loop over ysim columns */
     if( datum == MRI_short ) scar = DSET_ARRAY(cset,kk) ; /* output array */
     else                     fcar = DSET_ARRAY(cset,kk) ;
     ysar = MRI_FLOAT_PTR(ysim) + (kk * ysim->nx) ;     /* 1D data pointer */

     /* loop over voxels, correlate */

     for( ii=0 ; ii < nvox ; ii++ ){

       /* get time series to correlate */

       (void)THD_extract_array(ii,xset,0,xsar) ;             /* 3D data */
       for( jj=0 ; jj < nvals && xsar[jj]==0.0f ; jj++ ) ;      /* nada */
       if( jj == nvals ) continue ;                /* data was all zero */
       for( jj=0 ; jj < nvals ; jj++ ) ydar[jj] = ysar[jj] ; /* 1D data */

       val = corfun( nvals , xsar , ydar ) ; /* correlate! */

       if( datum == MRI_short ) scar[ii] = (short)(10000.0f*val) ;
       else                     fcar[ii] = val ;

     } /* end of loop over voxels */

   } /* end of loop over ysim columns */

   free(ydar) ; free(xsar) ;
 AFNI_OMP_END ;
 } /* end OpenMP */

   DSET_unload(xset) ;  /* no longer needful */

   /* finito */

   DSET_write(cset) ;
   INFO_message("Wrote dataset: %s\n",DSET_BRIKNAME(cset)) ;
   exit(0) ;
}
