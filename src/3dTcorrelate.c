#include "mrilib.h"

#define SPEARMAN 1
#define QUADRANT 2
#define PEARSON  3
#define KTAUB    4

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *xset , *yset , *cset ;
   int nopt=1 , method=PEARSON , do_autoclip=0 ;
   int nvox , nvals , ii , polort=1 ;
   MRI_IMAGE *xsim , *ysim ;
   float     *xsar , *ysar , *car ;
   char *prefix = "Tcorr" ;
   byte *mmm=NULL ;
   MRI_IMAGE *im_ort=NULL ;            /* 13 Mar 2003 */
   int nort=0 ; float **fort=NULL ;

   /*----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTcorrelate [options] xset yset\n"
             "Computes the correlation coefficient between corresponding voxel\n"
             "time series in two input 3D+time datasets 'xset' and 'yset', and\n"
             "stores the output in a new 1 sub-brick dataset.\n"
             "\n"
             "Options:\n"
             "  -pearson  = Correlation is the normal Pearson (product moment)\n"
             "                correlation coefficient [this is the default method].\n"
             "  -spearman = Correlation is the Spearman (rank) correlation\n"
             "                coefficient.\n"
             "  -quadrant = Correlation is the quadrant correlation coefficient.\n"
             "  -ktaub    = Correlation is Kendall's tau_b coefficient.\n"
             "\n"
             " http://en.wikipedia.org/wiki/Correlation\n"
             " http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient\n"
             " http://en.wikipedia.org/wiki/Spearman's_rank_correlation_coefficient\n"
             " http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient\n"
             "\n"
             "  -polort m = Remove polynomical trend of order 'm', for m=-1..3.\n"
             "                [default is m=1; removal is by least squares].\n"
             "                Using m=-1 means no detrending; this is only useful\n"
             "                for data/information that has been pre-processed.\n"
             "\n"
             "  -ort r.1D = Also detrend using the columns of the 1D file 'r.1D'.\n"
             "                Only one -ort option can be given.  If you want to use\n"
             "                more than one, create a temporary file using 1dcat.\n"
             "\n"
             "  -autoclip = Clip off low-intensity regions in the two datasets,\n"
             "  -automask =  so that the correlation is only computed between\n"
             "               high-intensity (presumably brain) voxels.  The\n"
             "               intensity level is determined the same way that\n"
             "               3dClipLevel works.\n"
             "\n"
             "  -prefix p = Save output into dataset with prefix 'p'\n"
             "               [default prefix is 'Tcorr'].\n"
             "\n"
             "Notes:\n"
             " * The output dataset is functional bucket type, with one\n"
             "    sub-brick, stored in floating point format.\n"
             " * Because both time series are detrended prior to correlation,\n"
             "    the results will not be identical to using FIM or FIM+ to\n"
             "    calculate correlations (whose ideal vector is not detrended).\n"
             " * This is a quick hack for Mike Beauchamp.  Thanks for you-know-what.\n"
             "\n"
             "-- RWCox - Aug 2001\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dTCorrelate main"); machdep(); AFNI_logger("3dTcorrelate",argc,argv);
   PRINT_VERSION("3dTcorrelate") ;

   /*-- option processing --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-ort") == 0 ){           /* 13 Mar 2003 */
        if( im_ort != NULL ){
          fprintf(stderr,"** Can't have multiple -ort options!\n"); exit(1);
        }
        im_ort = mri_read_1D( argv[++nopt] ) ;
        if( im_ort == NULL ){
          fprintf(stderr,"** Can't read 1D file %s\n",argv[nopt]); exit(1);
        }
        nort = im_ort->ny ;
        fort = (float **) malloc( sizeof(float *)*nort ) ;
        for( ii=0 ; ii < nort ; ii++ )
          fort[ii] = MRI_FLOAT_PTR(im_ort) + (ii*im_ort->nx) ;

        nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-autoclip") == 0 ||
          strcmp(argv[nopt],"-automask") == 0   ){

         do_autoclip = 1 ; nopt++ ; continue ;
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
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"** Illegal value after -prefix!\n");exit(1);
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-polort") == 0 ){
         char *cpt ;
         int val = strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < -1 || val > 3 ){
            fprintf(stderr,"** Illegal value after -polort!\n");exit(1);
         }
         polort = val ; nopt++ ; continue ;
      }

      fprintf(stderr,"** Illegal option: %s\n",argv[nopt]) ; exit(1) ;
   }

   /*-- open datasets, check for legality --*/

   if( nopt+1 >= argc ){
      fprintf(stderr,"*** Need 2 datasets on command line!?\n"); exit(1);
   }

   xset = THD_open_dataset( argv[nopt] ) ;
   if( xset == NULL ){
      fprintf(stderr,"** Can't open dataset %s\n",argv[nopt]); exit(1);
   }
   if( DSET_NUM_TIMES(xset) < 2 ){
      fprintf(stderr,"** Input dataset %s is not 3D+time\n",argv[nopt]); exit(1);
   }
   yset = THD_open_dataset( argv[++nopt] ) ;
   if( yset == NULL ){
      fprintf(stderr,"** Can't open dataset %s\n",argv[nopt]); exit(1);
   }
   if( DSET_NUM_TIMES(yset) != DSET_NUM_TIMES(xset) ){
      fprintf(stderr,"** Input dataset %s is different length than %s\n",
              argv[nopt],argv[nopt-1]) ;
      exit(1) ;
   }
   if( DSET_NVOX(yset) != DSET_NVOX(xset) ){
      fprintf(stderr,"** Input dataset %s is different size than %s\n",
              argv[nopt],argv[nopt-1]) ;
      exit(1) ;
   }
   if( im_ort != NULL && im_ort->nx < DSET_NUM_TIMES(xset) ){
      fprintf(stderr,"** Input datsets are longer than -ort file!\n"); exit(1);
   }
   if( !EQUIV_GRIDS(xset,yset) )
     WARNING_message("Grid mismatch between input datasets!") ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;
   DSET_load(yset) ; CHECK_LOAD_ERROR(yset) ;

   /*-- compute mask array, if desired --*/

   nvox = DSET_NVOX(xset) ; nvals = DSET_NVALS(xset) ;

   if( do_autoclip ){
      byte *xmm , *ymm ;
      xmm = THD_automask( xset ) ;
      ymm = THD_automask( yset ) ;
      mmm = (byte *) malloc(sizeof(byte)*nvox) ;
      for( ii=0 ; ii < nvox ; ii++ )
         mmm[ii] = ( xmm[ii] && ymm[ii] ) ;
      free(xmm) ; free(ymm) ;
      ii = THD_countmask( nvox , mmm ) ;
      fprintf(stderr,"++ %d voxels survive -autoclip\n",ii) ;
      if( ii == 0 ) exit(1) ;
   }

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

   EDIT_BRICK_TO_FICO(cset,0,nvals,1,polort+1+nort) ;  /* stat params */
   EDIT_BRICK_FACTOR(cset,0,0.0) ;                     /* to be safe  */

   switch( method ){                                   /* looks nice  */
      default:
      case PEARSON:  EDIT_BRICK_LABEL(cset,0,"Pear.Corr.") ; break ;
      case SPEARMAN: EDIT_BRICK_LABEL(cset,0,"Spmn.Corr.") ; break ;
      case QUADRANT: EDIT_BRICK_LABEL(cset,0,"Quad.Corr.") ; break ;
      case KTAUB:    EDIT_BRICK_LABEL(cset,0,"Taub.Corr.") ; break ;
   }

   EDIT_substitute_brick( cset , 0 , MRI_float , NULL ) ; /* make array  */
   car = DSET_ARRAY(cset,0) ;                             /* get array   */

   tross_Make_History( "3dTcorrelate" , argc,argv , cset ) ;

   /* loop over voxels, correlate */
   /* fprintf(stderr,"have %d voxels to work with, %d values/time series\n",nvox,nvals);*/
   for( ii=0 ; ii < nvox ; ii++ ){

      if( mmm != NULL && mmm[ii] == 0 ){  /* the easy case */
         car[ii] = 0.0 ; continue ;
      }

      /* get time series */

      xsim = THD_extract_series(ii,xset,0) ; xsar = MRI_FLOAT_PTR(xsim) ;
      ysim = THD_extract_series(ii,yset,0) ; ysar = MRI_FLOAT_PTR(ysim) ;

      (void)THD_generic_detrend_LSQ( nvals,xsar, polort, nort,fort,NULL ) ;  /* 13 Mar 2003 */
      (void)THD_generic_detrend_LSQ( nvals,ysar, polort, nort,fort,NULL ) ;

      switch( method ){                    /* correlate */
         default:
         case PEARSON:  car[ii] = THD_pearson_corr ( nvals,xsar,ysar ); break;
         case SPEARMAN: car[ii] = THD_spearman_corr( nvals,xsar,ysar ); break;
         case QUADRANT: car[ii] = THD_quadrant_corr( nvals,xsar,ysar ); break;
         case KTAUB:    car[ii] = THD_ktaub_corr   ( nvals,xsar,ysar ); break;
      }

      mri_free(xsim) ; mri_free(ysim) ;    /* toss time series */

   } /* end of loop over voxels */

   /* toss the other trash */

   DSET_unload(xset); DSET_unload(yset); if( mmm != NULL ) free(mmm);

   /* finito */

   DSET_write(cset) ;
   fprintf(stderr,"++ Wrote dataset: %s\n",DSET_BRIKNAME(cset)) ;
   exit(0) ;
}
