#include "mrilib.h"

#ifdef USE_OMP
#endif

/*--------------------------------------------------------------------------*/
/* adaptive = downweight things a long ways from median */

float adaptive_weighted_mean( int num , float *x )
{
   float med,mad, wt,wsum, xsum ; int ii ;

        if( num <= 0 || x == NULL ) return (0.0f) ;
   else if( num == 1              ) return (x[0]) ;
   else if( num == 2              ) return (0.5f*(x[0]+x[1])) ;

   qmedmad_float( num , x , &med , &mad ) ;
   if( mad <= 0.0f ) return (med) ;

   wsum = xsum = 0.0f ; mad = 0.56789f / mad ;
   for( ii=0 ; ii < num ; ii++ ){
     wt = mad*fabsf(x[ii]-med); wt = 1.0f / (1.0f+wt*wt*wt); wsum += wt;
     xsum += wt * x[ii] ;
   }
   return (xsum/wsum) ;
}

/*--------------------------------------------------------------------------*/

static int nXX=0 , nHH=0 ;
static float *aXX=NULL ;
static float *tvv=NULL ;
static int    ntv=0 ;

void setup_adaptive_filter( int hwid , int num )
{
   if( hwid > 0 ){
     nHH = hwid ; nXX = 2*nHH+1 ;
     aXX = (float *)realloc(aXX,sizeof(float)*nXX) ;
   } else if( hwid <= 0 && nHH > 0 ){
     nHH = nXX = 0 ;
     if( aXX != NULL ){ free(aXX) ; aXX = NULL ; }
   }
   if( num > 0 ){
     tvv = (float *)realloc(tvv,sizeof(float)*num) ; ntv = num ;
   } else if( num <= 0 && tvv != NULL ){
     free(tvv) ; tvv = NULL ; ntv = 0 ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/

void adaptive_filter( int num , float *vec )
{
   int ii,jj,kk , n1=num-1 ;

   if( num < 2 || vec == NULL || nHH <= 0 ) return ;

   if( aXX == NULL ){ aXX = (float *)malloc(sizeof(float)*nXX) ; }
   if( ntv < num ){ tvv = (float *)realloc(tvv,sizeof(float)*num) ; ntv = num ; }

   for( ii=0 ; ii < num ; ii++ ){
     for( jj=-nHH ; jj <= nHH ; jj++ ){
       kk = ii+jj ; if( kk < 0 ) kk = 0 ; else if( kk > n1 ) kk = n1 ;
       aXX[jj+nHH] = vec[kk] ;
     }
     tvv[ii] = adaptive_weighted_mean( nXX , aXX ) ;
   }

   memcpy(vec,tvv,sizeof(float)*num) ; return ;
}

/*--------------------------------------------------------------------------*/

static int polort=-1 ;

void polort_filter( int num , float *vec )
{
   THD_generic_detrend_LSQ( num , vec , polort , 0,NULL,NULL ) ;
   return ;
}

/*--------------------------------------------------------------------------*/
/* Array of filter functions to apply to time series */

static int           nffunc=0 ;
static generic_func **ffunc=NULL ;

#define ADD_FILTER(ff)                                                         \
 do{ ffunc = (generic_func **)realloc(ffunc,sizeof(generic_func)*(nffunc+1)) ; \
     ffunc[nffunc++] = (generic_func *)(ff) ;                                  \
 } while(0)

/*--------------------------------------------------------------------------*/

void FILTER_tsfunc( double tzero , double tdelta ,
                    int npts , float *ts , double ts_mean ,
                    double ts_slope , void *ud , int nbriks, float *val )
{
   int qq ;

   if( ts == NULL || val == NULL ) return ;  /* setup/cleanup calls */

   memcpy(val,ts,sizeof(float)*npts) ;

   for( qq=0 ; qq < nffunc ; qq++ )
     AFNI_CALL_VOID_2ARG( ffunc[qq] , int,npts , float *,val ) ;

   return ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *old_dset=NULL , *new_dset=NULL ;
   char *prefix = "Filtered" ;
   int hh=0 ;
   int nvals , nopt ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
      "\n"
      "3dTfilter takes as input a dataset, filters the time series in\n"
      "each voxel as ordered by the user, and outputs a new dataset.\n"
      "The data in each voxel is processed separately.\n"
      "\n"
      "The user (you?) specifies the filter functions to apply.\n"
      "They are applied in the order given on the command line:\n"
      "  -filter rank -filter adaptive:7\n"
      "means to do the following operations\n"
      "  (1) turn the data into ranks\n"
      "  (2) apply the adaptive mean filter to the ranks\n"
      "\n"
      "Notes:\n"
      "------\n"
      "** This program is a work in progress, and more capabilities\n"
      "   will be added as time allows, as the need arises, and as\n"
      "   the author's whims bubble to the surface of his febrile brain.\n"
      "\n"
      "** This program is for people who have Sisu.\n"
      "\n"
      "Options:\n"
      "--------\n"
      "\n"
      " -input inputdataset\n"
      "\n"
      " -prefix outputdataset\n"
      "\n"
      " -filter FunctionName\n"
      "     At least one '-filter' option is required!\n"
      "     The FunctionName values that you can give are:\n"
      "\n"
      "        rank       = smallest value is replaced by 0,\n"
      "                     next smallest value by 1, and so forth.\n"
      "                     ** This filter is pretty useless.\n"
      "\n"
      "        adaptive:H = adaptive mean filter with half-width of\n"
      "                     'H' time points (H > 0).\n"
      "                     ** At most one 'adaptive' filter can be used!\n"
      "                     ** The filter 'footprint' is 2*H+1 points.\n"
      "                     ** This filter does local smoothing over the\n"
      "                        'footprint', with values far away from\n"
      "                        the local median being weighted less.\n"
      "\n"
      "        detrend:P  = (least squares) detrend with polynomials of up\n"
      "                     order 'P' for P=0, 1, 2, ....\n"
      "                     ** At most one 'detrend' filter can be used!\n"
      "\n"
      "Example:\n"
      "--------\n"
      " 3dTfilter -input fred.nii -prefix fred.af.nii -filter adaptive:7\n"
      "\n"
      "-------\n"
      "Author: The Programmer with No Name\n"
      "-------\n"
      "\n"
     ) ;
     exit(0) ;
   }

   /* bureaucracy */

   mainENTRY("3dTfilter main"); machdep(); AFNI_logger("3dTfilter",argc,argv);
   PRINT_VERSION("3dTfilter"); AUTHOR("Thorby Baslim");


   /*--- scan command line for options ---*/

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*-- prefix --*/

     if( strcasecmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("%s needs an argument!",argv[nopt-1]);
       prefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("%s is not a valid prefix!",prefix);
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-input") == 0 ||
         strcasecmp(argv[nopt],"-inset") == 0   ){
       if( ++nopt >= argc ) ERROR_exit("%s needs an argument!",argv[nopt-1]);
       if( old_dset != NULL ) ERROR_exit("you can't have 2 input datasets!") ;
       old_dset = THD_open_dataset(argv[nopt]) ;
       CHECK_OPEN_ERROR(old_dset,argv[nopt]) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-filter") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("%s needs an argument!",argv[nopt-1]);

       if( strcasecmp(argv[nopt],"rank") == 0 ){
         ADD_FILTER(rank_order_float) ;
         INFO_message("Filter #%d = rank",nffunc) ;

       } else if( strncasecmp(argv[nopt],"adaptive:",9) == 0 ){
         char *cpt=argv[nopt]+9 ;
         if( hh > 0 )
           ERROR_exit("You can't use more than one 'adaptive' filter :(") ;
         if( !isdigit(*cpt) )
           ERROR_exit("'%s' is not a valid 'adaptive' filter name",argv[nopt]) ;
         hh = (int)strtod(cpt,NULL) ;
         if( hh > 29 )
           WARNING_message("Very long filter '%s' will be very slow",argv[nopt]) ;
         else if( hh <= 0 )
           ERROR_exit("'%s' is not a legal 'adaptive' filter name",argv[nopt]) ;
         ADD_FILTER(adaptive_filter) ;
         INFO_message("Filter #%d = adaptive:%d",nffunc,hh) ;

       } else if( strncasecmp(argv[nopt],"detrend:",8) == 0 ){
         char *cpt=argv[nopt]+8 ;
         if( polort > 0 )
           ERROR_exit("You can't use more than one 'detrend' filter :(") ;
         if( !isdigit(*cpt) )
           ERROR_exit("'%s' is not a valid 'detrend' filter name",argv[nopt]) ;
         polort = (int)strtod(cpt,NULL) ;
         if( polort < 0 )
           ERROR_exit("'%s' is not a legal 'detrend' filter name",argv[nopt]) ;
         ADD_FILTER(polort_filter) ;
         INFO_message("Filter #%d = detrend:%d",nffunc,polort) ;

       } else {
         ERROR_exit("Unkown filter type '%s'",argv[nopt]) ;
       }
       nopt++ ; continue ;
     }

     ERROR_exit("Unknown option: '%s'",argv[nopt]) ;
   }

   if( nffunc == 0 ) ERROR_exit("No -filter options given !? :(") ;

   if( old_dset == NULL ){
     if( nopt >= argc ) ERROR_exit("no input dataset?") ;
     old_dset = THD_open_dataset(argv[nopt]) ;
     CHECK_OPEN_ERROR(old_dset,argv[nopt]) ;
   }

   nvals = DSET_NVALS(old_dset) ;
   if( nvals < 2 ) ERROR_exit("Input dataset too short to filter!") ;

   if( hh > 0 ) setup_adaptive_filter( hh , nvals ) ;

   INFO_message("Load input dataset") ;

   DSET_load(old_dset) ; CHECK_LOAD_ERROR(old_dset) ;

   /** do the work **/

   INFO_message("Start processing") ;

   new_dset = MAKER_4D_to_typed_fbuc(
                    old_dset ,             /* input dataset */
                    prefix ,               /* output prefix */
                    MRI_float ,            /* output datum  */
                    0 ,                    /* ignore count  */
                    0 ,                    /* don't detrend */
                    nvals ,                /* number of briks */
                    FILTER_tsfunc ,        /* timeseries processor */
                    NULL,                  /* data for tsfunc */
                    NULL,                  /* mask */
                    0                      /* Allow auto scaling of output */
                 ) ;

   DSET_unload(old_dset) ;

   if( new_dset != NULL ){
     tross_Copy_History( old_dset , new_dset ) ;
     tross_Make_History( "3dTfilter" , argc,argv , new_dset ) ;
     if( DSET_NUM_TIMES(old_dset) > 1 )
       EDIT_dset_items( new_dset ,
                         ADN_ntt    , DSET_NVALS(old_dset) ,
                         ADN_ttorg  , DSET_TIMEORIGIN(old_dset) ,
                         ADN_ttdel  , DSET_TR(old_dset) ,
                         ADN_tunits , UNITS_SEC_TYPE ,
                       NULL ) ;
     DSET_write( new_dset ) ; WROTE_DSET( new_dset ) ;
   } else {
     ERROR_exit("Unable to compute output dataset!\n") ;
   }

   exit(0) ;
}
