#include "mrilib.h"

#define SPEARMAN 1
#define QUADRANT 2
#define PEARSON  3
#define KTAUB    4

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *xset , *cset ;
   int nopt=1 , method=PEARSON , do_autoclip=0 ;
   int nvox , nvals , ii , polort=1 ;
   MRI_IMAGE *xsim , *ysim ;
   float     *xsar , *ysar , *ydar , *car ;
   char *prefix = "Tcorr1D" ;
   byte *mmm=NULL ;

   /*----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTcorr1D [options] xset y1D\n"
             "Computes the correlation coefficient between each voxel time series\n"
             "the input 3D+time dataset 'xset' and the 1D time series file 'y1D',\n"
             "and stores the output in a new 1 sub-brick dataset.\n"
             "\n"
             "Options:\n"
             "  -pearson  = Correlation is the normal Pearson (product moment)\n"
             "                correlation coefficient [this is the default method].\n"
             "  -spearman = Correlation is the Spearman (rank) correlation\n"
             "                coefficient.\n"
             "  -quadrant = Correlation is the quadrant correlation coefficient.\n"
             "  -ktaub    = Correlation is Kendall's tau_b coefficient.\n"
             "\n"
             "  -prefix p = Save output into dataset with prefix 'p'\n"
             "               [default prefix is 'Tcorr1D'].\n"
             "\n"
             "Notes:\n"
             "* The output dataset is functional bucket type, with just one\n"
             "  sub-brick, stored in floating point format.\n"
             "* http://en.wikipedia.org/wiki/Correlation\n"
             "* http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient\n"
             "* http://en.wikipedia.org/wiki/Spearman's_rank_correlation_coefficient\n"
             "* http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient\n"
             "\n"
             "-- RWCox - Apr 2010\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dTcorr1D main"); machdep(); AFNI_logger("3dTcorr1D",argc,argv);
   PRINT_VERSION("3dTcorr1D") ;

   /*-- option processing --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

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

   xset = THD_open_dataset( argv[nopt] ) ;
   if( xset == NULL )
     ERROR_exit("Can't open dataset %s",argv[nopt]) ;

   nvals = DSET_NVALS(xset) ;
   if( nvals < 3 )
     ERROR_exit("Input dataset %s length is less than 3",argv[nopt]) ;

   ysim = mri_read_1D( argv[++nopt] ) ;
   if( ysim == NULL )
     ERROR_exit("Can't read 1D file %s",argv[nopt]) ;

   if( ysim->ny > 1 )
     WARNING_message("1D file %s has %d columns: only first one will be used!",
                     argv[nopt],ysim->ny) ;

   if( ysim->nx < nvals )
     ERROR_exit("1D file %s has %d time points, but dataset has %d values",
                argv[nopt],ysim->nx,nvals) ;
   else if( ysim->nx > nvals )
     WARNING_message("1D file %s has %d time points, dataset has %d",
                     argv[nopt],ysim->nx,nvals) ;

   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   nvox = DSET_NVOX(xset) ;

   /*-- create output dataset --*/

   cset = EDIT_empty_copy( xset ) ;
   EDIT_dset_items( cset ,
                      ADN_prefix    , prefix         ,
                      ADN_nvals     , 1              ,
                      ADN_ntt       , 0              , /* no time axis */
                      ADN_type      , HEAD_FUNC_TYPE ,
                      ADN_func_type , FUNC_BUCK_TYPE ,
                    ADN_none ) ;

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(cset)) ){
      fprintf(stderr,"** Output dataset %s already exists!\n",
              DSET_HEADNAME(cset)) ;
      exit(1) ;
   }

   EDIT_BRICK_TO_FICO(cset,0,nvals,1,1) ;  /* stat params */
   EDIT_BRICK_FACTOR(cset,0,0.0) ;         /* to be safe  */

   switch( method ){                                   /* looks nice  */
      default:
      case PEARSON:  EDIT_BRICK_LABEL(cset,0,"Pear.Corr.") ; break ;
      case SPEARMAN: EDIT_BRICK_LABEL(cset,0,"Spmn.Corr.") ; break ;
      case QUADRANT: EDIT_BRICK_LABEL(cset,0,"Quad.Corr.") ; break ;
      case KTAUB:    EDIT_BRICK_LABEL(cset,0,"Taub.Corr.") ; break ;
   }

   EDIT_substitute_brick( cset , 0 , MRI_float , NULL ) ; /* make array  */
   car = DSET_ARRAY(cset,0) ;                             /* get array   */

   tross_Make_History( "3dTcorr1D" , argc,argv , cset ) ;

   /* loop over voxels, correlate */

   ysar = MRI_FLOAT_PTR(ysim) ;
   ydar = (float *)malloc(sizeof(float)*nvals) ;

   for( ii=0 ; ii < nvox ; ii++ ){

      /* get time series */

      xsim = THD_extract_series(ii,xset,0) ; xsar = MRI_FLOAT_PTR(xsim) ;
      memcpy(ydar,ysar,sizeof(float)*nvals) ;

      switch( method ){                    /* correlate */
         default:
         case PEARSON:  car[ii] = THD_pearson_corr ( nvals,xsar,ydar ); break;
         case SPEARMAN: car[ii] = THD_spearman_corr( nvals,xsar,ydar ); break;
         case QUADRANT: car[ii] = THD_quadrant_corr( nvals,xsar,ydar ); break;
         case KTAUB:    car[ii] = THD_ktaub_corr   ( nvals,xsar,ydar ); break;
      }

      mri_free(xsim) ;

   } /* end of loop over voxels */

   /* toss the other trash */

   DSET_unload(xset) ;

   /* finito */

   DSET_write(cset) ;
   fprintf(stderr,"++ Wrote dataset: %s\n",DSET_BRIKNAME(cset)) ;
   exit(0) ;
}
