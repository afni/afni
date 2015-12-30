#include "mrilib.h"

/*----------------------------------------------------------------------------*/

static double cf_nor( double x ){ return 1.0-0.5*erfc(x/1.414213562373095); }

/*----------------------------------------------------------------------------*/

double anderson_darling_statistic( int npt, double *val, double (*cf)(double) )
{
   double *yyy , asum , ccc ; int ii ;

   if( npt < 5 || val == NULL || cf == NULL ) return 0.0 ;

   yyy = (double *)malloc(sizeof(double)*npt) ;
   memcpy( yyy , val , sizeof(double)*npt ) ;
   qsort_double( npt , yyy ) ;

   asum = 0.0 ;
   for( ii=0 ; ii < npt ; ii++ ){
     ccc   = cf(yyy[ii]) ;
     if( ccc > 0.0 && ccc < 1.0 )
       asum += (2*ii+1) * log(ccc) + (2*(npt-ii)-1) * log(1.0-ccc) ;
   }

   free(yyy) ; asum = -npt - asum / npt ; return asum ;
}

/*----------------------------------------------------------------------------*/

double anderson_darling_normal( int npt , double *xxx )
{
   double *vvv , xm , xq , ad ; int ii ;

   if( npt < 5 || xxx == NULL ) return 0.0 ;

   vvv = (double *)malloc(sizeof(double)*npt) ;
   memcpy( vvv , xxx , sizeof(double)*npt ) ;

   xm = 0.0 ;
   for( ii=0 ; ii < npt ; ii++ ) xm += vvv[ii] ;

   xm /= npt ; xq = 0.0 ;
   for( ii=0 ; ii < npt ; ii++ ) xq += (vvv[ii]-xm)*(vvv[ii]-xm) ;
   if( xq <= 0.0 ){ free(vvv) ; return 0.0 ; }

   xq = sqrt( (npt-1.0) / xq ) ;
   for( ii=0 ; ii < npt ; ii++ ) vvv[ii] = (vvv[ii]-xm) * xq ;

   ad = anderson_darling_statistic( npt , vvv , cf_nor ) ;
   ad *= ( 1.0 + 4.0/npt - 25.0/(npt*npt) ) ;
   free(vvv) ; return ad ;
}

/*----------------------------------------------------------------------------*/

#include "zgaussian.c"

float * anderson_darling_simulate( int npt , int ntrial )
{
   float *ad ; double *xxx ; int ii , jj ;

   if( npt < 5 || ntrial <= 0 ) return NULL ;

   ad  = (float * )malloc(sizeof(float)*ntrial) ;
   xxx = (double *)malloc(sizeof(double)*npt) ;
   for( jj=0 ; jj < ntrial ; jj++ ){
     for( ii=0 ; ii < npt ; ii++ )
       xxx[ii] = (double)(zgaussian()+zgaussian()+zgaussian()+1.0f) ;
     ad[jj] = - (float)anderson_darling_normal( npt , xxx ) ;
   }
   qsort_float( ntrial , ad ) ;
   for( jj=0 ; jj < ntrial ; jj++ ) ad[jj] = -ad[jj] ;
   return ad ;
}

/*----------------------------------------------------------------------------*/

void anderson_darling_expify( int naval, float *aval, int natr, float *atr, int dopval )
{
   float *qaval , avv , pfac,pval,dd ; int *kaval , jj,kk ;

   if( naval <= 0 || aval == NULL || natr < 100 || atr == NULL ) return ;

   qaval = (float *)malloc(sizeof(float)*naval) ;
   kaval = (int *  )malloc(sizeof(int)  *naval) ;
   for( kk=0 ; kk < naval ; kk++ ){ qaval[kk] = -aval[kk]; kaval[kk] = kk; }

   qsort_floatint( naval , qaval , kaval ) ;
   for( kk=0 ; kk < naval ; kk++ ) qaval[kk] = -qaval[kk];

   pfac = 1.0f / (1.0f+natr) ;

   /* process aval[] entries larger than the largest atr value */

   for( kk=0 ; kk < naval ; kk++ ){
     if( qaval[kk] >= atr[0] ){
       pval = pfac * atr[0] / qaval[kk] ;         /* make up a small p-value */
       aval[kaval[kk]] = (dopval) ? pval : -logf(pval) ;   /* convert to exp */
     } else {
       break ;
     }
   }

   /* process the remaining aval[] entries, starting at aval[kk] */

   for( jj=0 ; kk < naval ; kk++ ){
     avv = qaval[kk] ;
     for( ; jj < natr && avv <= atr[jj] ; jj++ ) ; /* get bounding atr[] vals */
     if( jj == natr ) break ;
     pval = jj ; dd = atr[jj] - atr[jj-1] ;
     if( dd != 0.0f ) pval += (atr[jj-1]-avv) / dd ;    /* interpolate a pval */
     pval *= pfac ;
     aval[kaval[kk]] = (dopval) ? pval : -logf(pval) ;      /* convert to exp */
   }
   /** ININFO_message("kk = %d/%d after middle step",kk,naval) ; **/

   dd = -logf(1.0f-pfac) ;
   for( ; kk < naval ; kk++ ) aval[kaval[kk]] = dd ;

   free(kaval) ; free(qaval) ; return ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset ;
   int nvox , nvals , nopt=1 , ii,jj , nbad , ntr , convertize=1 , dopval=0 ;
   char *prefix="NormTest" ;
   float *avar , *dval , *atr ; double *eval ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
       "Program: 3dNormalityTest\n"
       "\n"
       "* This program tests the input values at each voxel for normality,\n"
       "  using the Anderson-Darling method:\n"
       "    http://en.wikipedia.org/wiki/Anderson-Darling_test\n"
       "\n"
       "* Each voxel must have at least 5 values (sub-bricks).\n"
       "\n"
       "* The resulting dataset has the Anderson-Darling statistic converted\n"
       "  to an exponentially distributed variable, so it can be thresholded\n"
       "  with the AFNI slider and display a nominal p-value below.  If you\n"
       "  want the A-D statistic un-converted, use the '-noexp' option.\n"
       "\n"
       "* Conversion of the A-D statistic to a p-value is done via simulation\n"
       "  of the null distribution.\n"
       "\n"
       "OPTIONS:\n"
       "--------\n"
       " -input dset  = Specifies the input dataset.\n"
       "                Alternatively, the input dataset can be given as the\n"
       "                last argument on the command line, after all other\n"
       "                options.\n"
       "\n"
       " -prefix ppp  = Specifies the name for the output dataset.\n"
       "\n"
       " -noexp       = Do not convert the A-D statistic to an exponentially\n"
       "                distributed value -- just leave the raw A-D score in\n"
       "                the output dataset.\n"
       " -pval        = Output the results as a pure (estimated) p-value.\n"
       "\n"
       "EXAMPLES:\n"
       "---------\n"
       "(1) Simulate a 2D square dataset with the values being normal on one\n"
       "edge and exponentially distributed on the other, and mixed in-between.\n"
       "\n"
       "  3dUndump -dimen 101 101 1 -prefix UUU\n"
       "  3dcalc -datum float -a UUU+orig -b '1D: 0 0 0 0 0 0 0 0 0 0' -prefix NNN \\\n"
       "         -expr 'i*gran(0,1.4)+(100-i)*eran(4)'\n"
       "  rm -f UUU+orig.*\n"
       "  3dNormalityTest -prefix Ntest -input NNN+orig\n"
       "  afni -com 'OPEN_WINDOW axialimage' Ntest+orig\n"
       "\n"
       "In the above script, the UUU+orig dataset is created just to provide a spatial\n"
       "template for 3dcalc.  The '1D: 0 ... 0' input to 3dcalc is a time template\n"
       "to create a dataset with 10 time points.  The values are random deviates,\n"
       "ranging from pure Gaussian where i=100 to pure exponential at i=0.\n"
       "\n"
       "(2) Simulate a single logistic random variable into a 1D file and compute\n"
       "the A-D nominal p-value:\n"
       "\n"
       "  1deval -num 200 -expr 'lran(2)' > logg.1D\n"
       "  3dNormalityTest -input logg.1D\\' -prefix stdout: -pval\n"
       "\n"
       "Note the necessity to transpose the logg.1D file (with the \\' operator),\n"
       "since 3D programs interpret each 1D file row as a voxel time series.\n"
       "\n"
       "++ March 2012 -- by The Ghost of Carl Friedrich Gauss\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*--- deal with options ---*/

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcasecmp(argv[nopt],"-noexp") == 0 ){
       convertize = 0 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-pval") == 0 ){
       convertize = 1 ; dopval = 1 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-input") == 0 ){
       if( inset != NULL ) ERROR_exit("Can't have 2 -input options!") ;
       if( ++nopt >= argc ) ERROR_exit("Need an argument after -input!") ;
       inset = THD_open_dataset(argv[nopt]) ;
       CHECK_OPEN_ERROR(inset,argv[nopt]) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("Need an argument after -prefix!") ;
       prefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("-prefix '%s' has illegal characters :-(",prefix) ;
       nopt++ ; continue ;
     }

     ERROR_message("Unknown option: %s",argv[nopt]) ;
     suggest_best_prog_option(argv[0], argv[nopt]);
     exit(1);

   } /* end of loop over options */

   mainENTRY("3dNormalityTest main"); machdep(); AFNI_logger("3dNormalityTest",argc,argv);
   PRINT_VERSION("3dNormalityTest"); AUTHOR("The Ghost of Gauss");

   /*--- deal with input ---*/

   if( inset == NULL ){
     if( nopt < argc ){
       inset = THD_open_dataset(argv[nopt]) ;
       CHECK_OPEN_ERROR(inset,argv[nopt]) ;
     } else {
       ERROR_exit("No input dataset???") ;
     }
   }
   nvals = DSET_NVALS(inset) ;
   nvox  = DSET_NVOX(inset) ;
   if( nvals < 5 )
     ERROR_exit("Input dataset '%s' has %d sub-bricks, less than 5 :-(",
                    argv[nopt] , nvals ) ;

   EDIT_floatize_dataset(inset) ;
   if( DSET_pure_type(inset) != MRI_float )
     ERROR_exit("Couldn't load input dataset properly :-(") ;

   /*--- create output dataset ---*/

   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset ,
                      ADN_prefix , prefix ,
                      ADN_nvals  , 1 ,
                      ADN_ntt    , 0 ,
                      ADN_type      , ISHEAD(inset) ? HEAD_FUNC_TYPE :
                                                      GEN_FUNC_TYPE ,
                      ADN_func_type , FUNC_BUCK_TYPE ,
                    ADN_none ) ;
   EDIT_substitute_brick( outset , 0 , MRI_float , NULL ) ;
   avar = DSET_BRICK_ARRAY(outset,0) ;
   if( avar == NULL ) ERROR_exit("Can't create output dataset array?!") ;

   dval = (float * )malloc(sizeof(float )*nvals) ;
   eval = (double *)malloc(sizeof(double)*nvals) ;

   INFO_message("Computing Anderson-Darling statistics for all voxels") ;

   for( nbad=ii=0 ; ii < nvox ; ii++ ){
     THD_extract_array( ii , inset , 0 , dval ) ;
     for( jj=1 ; jj < nvals ; jj++ ) if( dval[jj] != dval[0] ) break ;
     if( jj == nvals ){ avar[ii] = 0.0f ; nbad++ ; continue ; }
     for( jj=0 ; jj < nvals ; jj++ ) eval[jj] = (double)dval[jj] ;
     avar[ii] = anderson_darling_normal( nvals , eval ) ;
   }

   if( nbad > 0 )
     ININFO_message("%d voxel%s were constant ==> skipped test there",
                    nbad , (nbad > 1) ? "s" : "\0" ) ;

   DSET_delete(inset) ; free(dval) ; free(eval) ;

   if( convertize ){
     ntr = 9*nvox ;
          if( ntr <  300000 ) ntr =  300000 ;
     else if( ntr > 3000000 ) ntr = 3000000 ;

     INFO_message("Simulating A-D null distribution: %d trials",ntr) ;

     atr = anderson_darling_simulate( nvals , ntr ) ;
     if( atr == NULL ) ERROR_exit("Simulation failed!?") ;
     /** ININFO_message("range %g .. %g",atr[0],atr[ntr-1]) ; **/

     if( dopval )
       INFO_message("Converting A-D statistics to p-values") ;
     else
       INFO_message("Converting A-D statistics to exponential distribution") ;

     anderson_darling_expify( nvox , avar , ntr , atr , dopval ) ;
     free(atr) ;

     EDIT_BRICK_TO_FIGT( outset , 0 , 1.0f , 1.0f ) ;
     EDIT_BRICK_LABEL  ( outset , 0 , "A-D_expify" ) ;
   } else {
     EDIT_BRICK_LABEL  ( outset , 0 , "A-D_statistic" ) ;
   }

   DSET_write( outset ) ;
   WROTE_DSET( outset ) ;
   exit(0) ;
}
