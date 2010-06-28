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
   float (*corfun)(int,float *,float *) = NULL ;  /* ptr to correlation function */
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , nmask=0 ;

   /*----*/

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
             "\n"
             "NOTES:\n"
             "* The output dataset is functional bucket type, with one sub-brick\n"
             "   per column of the input y1D file.\n"
             "* No detrending, blurring, or other pre-processing options are available;\n"
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
             "         - Jun 2010: Multiple y1D columns; OpenMP; -short; -mask.\n"
            ) ;
      PRINT_AFNI_OMP_USAGE("3dTcorr1D",NULL) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dTcorr1D main"); machdep(); AFNI_logger("3dTcorr1D",argc,argv);
   PRINT_VERSION("3dTcorr1D") ;

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
        method = PEARSON ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-spearman") == 0 || strcasecmp(argv[nopt],"-rank") == 0 ){
        method = SPEARMAN ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-quadrant") == 0 ){
        method = QUADRANT ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-ktaub") == 0 || strcasecmp(argv[nopt],"-taub") == 0 ){
        method = KTAUB ; nopt++ ; continue ;
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
     INFO_message("1D file %s has %d columns: correlating with ALL of them!",
                   ynam,ny) ;

   ININFO_message("loading dataset %s into memory",DSET_BRIKNAME(xset)) ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   nvox = DSET_NVOX(xset) ;
   if( mask == NULL ) nmask = nvox ;

   /*-- create output dataset --*/

   cset = EDIT_empty_copy( xset ) ;
   EDIT_dset_items( cset ,
                      ADN_prefix    , prefix         ,
                      ADN_nvals     , ny             ,
                      ADN_ntt       , 0              , /* no time axis */
                      ADN_type      , HEAD_FUNC_TYPE ,
                      ADN_func_type , FUNC_BUCK_TYPE ,
                    ADN_none ) ;

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(cset)) )
     ERROR_exit("Output dataset %s already exists!",DSET_HEADNAME(cset)) ;

        if( ny <   10 ) kk = 1 ;  /* number of digits for */
   else if( ny <  100 ) kk = 2 ;  /* brick label string */
   else if( ny < 1000 ) kk = 3 ;
   else                 kk = 4 ;
   switch( method ){              /* brick label string format */
     default:
     case PEARSON:  sprintf(fmt,"PearCorr#%%%dd",kk) ; break ;
     case SPEARMAN: sprintf(fmt,"SpmnCorr#%%%dd",kk) ; break ;
     case QUADRANT: sprintf(fmt,"QuadCorr#%%%dd",kk) ; break ;
     case KTAUB:    sprintf(fmt,"TaubCorr#%%%dd",kk) ; break ;
   }
   if( datum == MRI_short ) cfac = 0.0001f ;  /* scale factor for -short */

   /* for each sub-brick in output file */

   for( kk=0 ; kk < ny ; kk++ ){
     EDIT_substitute_brick(cset,kk,datum,NULL) ; /* make brick */
     EDIT_BRICK_TO_FICO(cset,kk,nvals,1,1) ;    /* stat params */
     EDIT_BRICK_FACTOR(cset,kk,cfac) ;     /* set brick factor */
     sprintf(str,fmt,kk) ;
     EDIT_BRICK_LABEL(cset,kk,str) ;         /* labelize brick */
   }

   tross_Make_History( "3dTcorr1D" , argc,argv , cset ) ;

   switch( method ){               /* set correlation function */
     default:
     case PEARSON:  corfun = THD_pearson_corr  ; break ;
     case SPEARMAN: corfun = THD_spearman_corr ; break ;
     case QUADRANT: corfun = THD_quadrant_corr ; break ;
     case KTAUB:    corfun = THD_ktaub_corr    ; break ;
   }

   /* 27 Jun 2010: OpenMP-ize over columns in ysim */

#pragma omp parallel if( ny > 1 )
 { float *ysar, *xsar, *fcar, *ydar, val ; int ii, kk, jj ; short *scar ;
 AFNI_OMP_START ;

#ifdef USE_OMP
   if( omp_get_thread_num() == 0 )
     INFO_message("Start correlations: %d voxels X %d time series(%d); %d threads",
                  nmask , ny , nvals , omp_get_num_threads() ) ;
#else
   INFO_message("Start correlations: %d voxels X %d time series(%d)",nmask,ny,nvals) ;
#endif

   ydar = (float *)malloc(sizeof(float)*nvals) ;  /* 1D data duplicate */
   xsar = (float *)malloc(sizeof(float)*nvals) ;  /* 3D data duplicate */

   /* 27 Jun 2010: loop over columns in ysim */

#pragma omp for
   for( kk=0 ; kk < ny ; kk++ ){  /* loop over ysim columns */
     if( datum == MRI_short ) scar = DSET_ARRAY(cset,kk) ; /* output array */
     else                     fcar = DSET_ARRAY(cset,kk) ;
     ysar = MRI_FLOAT_PTR(ysim) + (kk * ysim->nx) ;     /* 1D data pointer */

     /* loop over voxels, correlate */

     for( ii=0 ; ii < nvox ; ii++ ){

       if( mask != NULL && mask[ii] == 0 ) continue ;    /* skip this'n */

       /* get time series to correlate */

       (void)THD_extract_array(ii,xset,0,xsar) ;             /* 3D data */
       for( jj=0 ; jj < nvals && xsar[jj]==0.0f ; jj++ ) ;      /* nada */
       if( jj == nvals ) continue ;                /* data was all zero */
       for( jj=0 ; jj < nvals ; jj++ ) ydar[jj] = ysar[jj] ; /* 1D data */

       val = corfun( nvals , xsar , ydar ) ;         /* !! correlate !! */

       if( datum == MRI_short ) scar[ii] = (short)(10000.4f*val) ;
       else                     fcar[ii] = val ;

     } /* end of loop over voxels */

#pragma omp critical
     { if( ny > 1 ) fprintf(stderr,"[%d]",kk) ; }
   } /* end of loop over ysim columns */

   free(ydar) ; free(xsar) ;
 AFNI_OMP_END ;
 } /* end OpenMP */

   if( ny > 1 ) fprintf(stderr,"\n") ;
   DSET_unload(xset) ;  /* no longer needful */

   /* finito */

   DSET_write(cset) ;
   INFO_message("Wrote dataset: %s\n",DSET_BRIKNAME(cset)) ;
   exit(0) ;
}
