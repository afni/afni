#include "mrilib.h"

#ifdef USE_OMP
#endif

/*--------------------------------------------------------------------------*/
/* adaptive = downweight things a long ways from median */

float adaptive_weighted_mean( int num , float *x )
{
   float med,mad, wt,wsum, xsum ; int ii ;

ENTRY("adaptive_weighted_mean") ;

        if( num <= 0 || x == NULL ) RETURN (0.0f) ;
   else if( num == 1              ) RETURN (x[0]) ;
   else if( num == 2              ) RETURN (0.5f*(x[0]+x[1])) ;

   qmedmad_float( num , x , &med , &mad ) ;
   if( mad <= 0.0f ) RETURN (med) ;

   wsum = xsum = 0.0f ; mad = 0.56789f / mad ;
   for( ii=0 ; ii < num ; ii++ ){
     wt = mad*fabsf(x[ii]-med); wt = 1.0f / (1.0f+wt*wt*wt); wsum += wt;
     xsum += wt * x[ii] ;
   }
   RETURN (xsum/wsum) ;
}

/*--------------------------------------------------------------------------*/

static int nXX=0 , nHH_adapt=0 ;
static float *aXX=NULL ;
static float *tvv=NULL , *uvv=NULL ;
static int    ntv=0 ;

void setup_adaptive_filter( int hwid , int num )
{
ENTRY("setup_adaptive_filter") ;
   if( hwid > 0 && num > 0 && num < 4*hwid+1 ){
     ERROR_message(
       "Illegal setup_adaptive_filter: 4*hwid+1=%d > num=%d",4*hwid+1,num) ;
     EXRETURN ;
   }
   if( hwid > 0 ){
     nHH_adapt = hwid ; nXX = 2*nHH_adapt+1 ;
     aXX = (float *)realloc(aXX,sizeof(float)*nXX) ;
   } else if( hwid <= 0 && nHH_adapt > 0 ){  /* cleanup */
     nHH_adapt = nXX = 0 ;
     if( aXX != NULL ){ free(aXX) ; aXX = NULL ; }
   }
   if( hwid > 0 && num > 0 ){
     ntv = num + nXX ;
     tvv = (float *)realloc(tvv,sizeof(float)*ntv) ;
     uvv = (float *)realloc(uvv,sizeof(float)*ntv) ;
   } else if( hwid <= 0 || num <= 0 ){
     if( tvv != NULL ) free(tvv) ;
     if( uvv != NULL ) free(uvv) ;
     tvv = uvv = NULL ; ntv = 0 ;
   }
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

static int dosub=0 ;

void FILTER_adaptive( int num , float *vec )
{
   int ii,jj,kk , nn1=num-1 , nn2=num-2 , nph = num+nHH_adapt ;

ENTRY("FILTER_adaptive") ;

   if( num < 2 || vec == NULL || nHH_adapt <= 0 ) EXRETURN ;

   if( aXX == NULL ){ aXX = (float *)malloc(sizeof(float)*nXX) ; }

   if( ntv < num+nXX ){
     ntv = num+nXX ;
     tvv = (float *)realloc(tvv,sizeof(float)*ntv) ;
     uvv = (float *)realloc(uvv,sizeof(float)*ntv) ;
   }

   memcpy( tvv+nHH_adapt , vec , sizeof(float)*num ) ;
   for( ii=0 ; ii < nHH_adapt ; ii++ ){  /* end reflections */
     tvv[ii]     = vec[nHH_adapt-ii] ;
     tvv[nph+ii] = vec[nn2-ii] ;
   }

   for( ii=0 ; ii < num ; ii++ ){
     memcpy( aXX , tvv+ii , sizeof(float)*nXX ) ;
     uvv[ii] = adaptive_weighted_mean( nXX , aXX ) ;
   }

   if( dosub ){
     for( ii=0 ; ii < num ; ii++ ) vec[ii] -= uvv[ii] ;
   } else {
     memcpy(vec,uvv,sizeof(float)*num) ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

static int polort=-1 ;

void FILTER_polort( int num , float *vec )
{
   THD_generic_detrend_LSQ( num , vec , polort , 0,NULL,NULL ) ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* Despiking filter */

#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)

#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b)

static float *deswks = NULL ;

/*--- fast median of 9 values ---*/

static INLINE float median9f(float *p)
{
    register float temp ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[1]) ; SORT2(p[3],p[4]) ; SORT2(p[6],p[7]) ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[3]) ; SORT2(p[5],p[8]) ; SORT2(p[4],p[7]) ;
    SORT2(p[3],p[6]) ; SORT2(p[1],p[4]) ; SORT2(p[2],p[5]) ;
    SORT2(p[4],p[7]) ; SORT2(p[4],p[2]) ; SORT2(p[6],p[4]) ;
    SORT2(p[4],p[2]) ; return(p[4]) ;
}

/*--- get the local median and MAD of values vec[j-4 .. j+4] ---*/

#undef  mead9
#define mead9(j)                                               \
 { float qqq[9] ; int jj = (j)-4 ;                             \
   if( jj < 0 ) jj = 0; else if( jj+9 > num ) jj = num-9;      \
   qqq[0] = vec[jj+0]; qqq[1] = vec[jj+1]; qqq[2] = vec[jj+2]; \
   qqq[3] = vec[jj+3]; qqq[4] = vec[jj+4]; qqq[5] = vec[jj+5]; \
   qqq[6] = vec[jj+6]; qqq[7] = vec[jj+7]; qqq[8] = vec[jj+8]; \
   med    = median9f(qqq);     qqq[0] = fabsf(qqq[0]-med);     \
   qqq[1] = fabsf(qqq[1]-med); qqq[2] = fabsf(qqq[2]-med);     \
   qqq[3] = fabsf(qqq[3]-med); qqq[4] = fabsf(qqq[4]-med);     \
   qqq[5] = fabsf(qqq[5]-med); qqq[6] = fabsf(qqq[6]-med);     \
   qqq[7] = fabsf(qqq[7]-med); qqq[8] = fabsf(qqq[8]-med);     \
   mad    = median9f(qqq); }

/*-------------------------------------------------------------------------*/
/*! Remove spikes from a time series, in a very simplistic way.
    Return value is the number of spikes that were squashed [RWCox].
*//*-----------------------------------------------------------------------*/

void DES_despike9( int num , float *vec )
{
   int ii , nsp ; float *zma,*zme , med,mad,val ;

   if( num < 9 || vec == NULL ) return ; /* nada */

   if( deswks == NULL ) deswks = (float *)malloc(sizeof(float)*(4*num)) ;

   zme = deswks ; zma = zme + num ;

   for( ii=0 ; ii < num ; ii++ ){
     mead9(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ;
   if( mad <= 0.0f ) return ;
   mad *= 6.789f ;  /* threshold value */

   for( nsp=ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ){ vec[ii] = zme[ii]; nsp++; }

   return ;
}
#undef mead9

/*----------------------------------------------------------------------------*/
/* Similar code to the above, but for spans of length 25 instead of 9 */

static INLINE float median25f(float *p)
{
    register float temp ;
    SORT2(p[0], p[1]) ;   SORT2(p[3], p[4]) ;   SORT2(p[2], p[4]) ;
    SORT2(p[2], p[3]) ;   SORT2(p[6], p[7]) ;   SORT2(p[5], p[7]) ;
    SORT2(p[5], p[6]) ;   SORT2(p[9], p[10]) ;  SORT2(p[8], p[10]) ;
    SORT2(p[8], p[9]) ;   SORT2(p[12], p[13]) ; SORT2(p[11], p[13]) ;
    SORT2(p[11], p[12]) ; SORT2(p[15], p[16]) ; SORT2(p[14], p[16]) ;
    SORT2(p[14], p[15]) ; SORT2(p[18], p[19]) ; SORT2(p[17], p[19]) ;
    SORT2(p[17], p[18]) ; SORT2(p[21], p[22]) ; SORT2(p[20], p[22]) ;
    SORT2(p[20], p[21]) ; SORT2(p[23], p[24]) ; SORT2(p[2], p[5]) ;
    SORT2(p[3], p[6]) ;   SORT2(p[0], p[6]) ;   SORT2(p[0], p[3]) ;
    SORT2(p[4], p[7]) ;   SORT2(p[1], p[7]) ;   SORT2(p[1], p[4]) ;
    SORT2(p[11], p[14]) ; SORT2(p[8], p[14]) ;  SORT2(p[8], p[11]) ;
    SORT2(p[12], p[15]) ; SORT2(p[9], p[15]) ;  SORT2(p[9], p[12]) ;
    SORT2(p[13], p[16]) ; SORT2(p[10], p[16]) ; SORT2(p[10], p[13]) ;
    SORT2(p[20], p[23]) ; SORT2(p[17], p[23]) ; SORT2(p[17], p[20]) ;
    SORT2(p[21], p[24]) ; SORT2(p[18], p[24]) ; SORT2(p[18], p[21]) ;
    SORT2(p[19], p[22]) ; SORT2(p[8], p[17]) ;  SORT2(p[9], p[18]) ;
    SORT2(p[0], p[18]) ;  SORT2(p[0], p[9]) ;   SORT2(p[10], p[19]) ;
    SORT2(p[1], p[19]) ;  SORT2(p[1], p[10]) ;  SORT2(p[11], p[20]) ;
    SORT2(p[2], p[20]) ;  SORT2(p[2], p[11]) ;  SORT2(p[12], p[21]) ;
    SORT2(p[3], p[21]) ;  SORT2(p[3], p[12]) ;  SORT2(p[13], p[22]) ;
    SORT2(p[4], p[22]) ;  SORT2(p[4], p[13]) ;  SORT2(p[14], p[23]) ;
    SORT2(p[5], p[23]) ;  SORT2(p[5], p[14]) ;  SORT2(p[15], p[24]) ;
    SORT2(p[6], p[24]) ;  SORT2(p[6], p[15]) ;  SORT2(p[7], p[16]) ;
    SORT2(p[7], p[19]) ;  SORT2(p[13], p[21]) ; SORT2(p[15], p[23]) ;
    SORT2(p[7], p[13]) ;  SORT2(p[7], p[15]) ;  SORT2(p[1], p[9]) ;
    SORT2(p[3], p[11]) ;  SORT2(p[5], p[17]) ;  SORT2(p[11], p[17]) ;
    SORT2(p[9], p[17]) ;  SORT2(p[4], p[10]) ;  SORT2(p[6], p[12]) ;
    SORT2(p[7], p[14]) ;  SORT2(p[4], p[6]) ;   SORT2(p[4], p[7]) ;
    SORT2(p[12], p[14]) ; SORT2(p[10], p[14]) ; SORT2(p[6], p[7]) ;
    SORT2(p[10], p[12]) ; SORT2(p[6], p[10]) ;  SORT2(p[6], p[17]) ;
    SORT2(p[12], p[17]) ; SORT2(p[7], p[17]) ;  SORT2(p[7], p[10]) ;
    SORT2(p[12], p[18]) ; SORT2(p[7], p[12]) ;  SORT2(p[10], p[18]) ;
    SORT2(p[12], p[20]) ; SORT2(p[10], p[20]) ; SORT2(p[10], p[12]) ;
    return (p[12]);
}

/*--- get the local median and MAD of values vec[j-12 .. j+12] ---*/

#undef  mead25
#define mead25(j)                                              \
 { float qqq[25] ; int jj=(j)-12 ; register int pp;            \
   if( jj < 0 ) jj = 0; else if( jj+25 > num ) jj = num-25;    \
   for( pp=0 ; pp < 25 ; pp++ ) qqq[pp] = vec[jj+pp] ;         \
   med = median25f(qqq) ;                                      \
   for( pp=0 ; pp < 25 ; pp++ ) qqq[pp] = fabsf(qqq[pp]-med) ; \
   mad = median25f(qqq); }

void DES_despike25( int num , float *vec )
{
   int ii , nsp ; float *zma,*zme , med,mad,val ;

   if( deswks == NULL ) deswks = (float *)malloc(sizeof(float)*(4*num)) ;

   if( vec == NULL ) return ;
   if( num <  25   ) { DES_despike9(num,vec) ; return ; }

   zme = deswks ; zma = zme + num ;

   for( ii=0 ; ii < num ; ii++ ){
     mead25(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ;
   if( mad <= 0.0f ) return ;
   mad *= 6.234f ;  /* threshold value */

   for( nsp=ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ){ vec[ii] = zme[ii]; nsp++; }

   return ;
}
#undef mead25
#undef SORT2
#undef SWAP

/*--------------- Generalized to arbitrary widths ----------------*/

#undef  meadHH
#define meadHH(j)                                               \
 { int jj=(j)-hwid ; register int pp;                           \
   if( jj < 0 ) jj = 0; else if( jj+nqqq > num ) jj = num-nqqq; \
   for( pp=0 ; pp < nqqq ; pp++ ) qqq[pp] = vec[jj+pp] ;        \
   qmedmad_float(nqqq,qqq,&med,&mad) ;                          \
 }

void DES_despikeHH( int num , float *vec , int hwid )
{
   int ii , nsp ; float *zma,*zme , med,mad,val ;
   static float *qqq=NULL ; static int nhold=0 , nqqq=0 ;

ENTRY("DES_despikeHH") ;

   if( vec == NULL || num < 9 || hwid < 4 ) EXRETURN ;

   if( deswks == NULL ) deswks = (float *)malloc(sizeof(float)*(4*num)) ;

   if( hwid == 12 ){
     DES_despike25(num,vec) ; EXRETURN ;
   } else if( hwid == 4 ){
     DES_despike9(num,vec) ; EXRETURN ;
   }

   if( hwid > nhold ){
     nhold = hwid ;
     qqq = (float *)realloc(qqq,sizeof(float)*(2*nhold+1)) ;
   }
   nqqq = 2*hwid+1 ;

   zme = deswks ; zma = zme + num ;

   for( ii=0 ; ii < num ; ii++ ){
     meadHH(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ;
   if( mad <= 0.0f ) EXRETURN ;
   mad *= 6.234f ;  /* threshold value */

   for( nsp=ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ){ vec[ii] = zme[ii]; nsp++; }

   EXRETURN ;
}
#undef meadHH
#undef SORT2
#undef SWAP

static int nHH_despike=0 ;

void FILTER_despikeHH( int num , float *vec )
{
   DES_despikeHH( num , vec , nHH_despike ) ;
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

/*----------------------------------------------------------------------------*/

static int vstep = 6666 ;

static void vstep_print(void)   /* pacifier */
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ; vn++ ;
}

/*--------------------------------------------------------------------------*/

void FILTER_tsfunc( double tzero , double tdelta ,
                    int npts , float *ts , double ts_mean ,
                    double ts_slope , void *ud , int nbriks, float *val )
{
   int qq ; static int ncalls=0 ;

   if( ts == NULL || val == NULL ) return ;  /* setup/cleanup calls */

   memcpy(val,ts,sizeof(float)*npts) ;

   for( qq=1 ; qq < npts ; qq++ ) if( val[qq] != val[0] ) break ;
   if( qq == npts ) return ;

   ncalls++ ; if( ncalls%vstep == 77 ) vstep_print() ;

   for( qq=0 ; qq < nffunc ; qq++ )
     AFNI_CALL_VOID_2ARG( ffunc[qq] , int,npts , float *,val ) ;

   return ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *old_dset=NULL , *new_dset=NULL ;
   char *prefix = "Filtered" ;
   int hwid=0 ;
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
      "        adetrend:H = apply adaptive mean filter with half-width\n"
      "                     of 'H' time points to get a local baseline,\n"
      "                     then subtract this baseline from the actual\n"
      "                     data, to provide an adaptive detrending.\n"
      "                     ** At most one 'adaptive' OR 'adetrend' filter\n"
      "                        can be used.\n"
      "\n"
      "        despike    = apply the 'NEW25' despiking algorithm, as in\n"
      "                     program 3dDespike.\n"
      "\n"
      "        despike:H  = apply the despiking algorithm over a window\n"
      "                     of half-with 'H' time points (667 > H > 3).\n"
      "                     ** H=12 is the same as 'despike'.\n"
      "                     ** At most one 'despike' filter can be used.\n"
      "\n"
      "        detrend:P  = (least squares) detrend with polynomials of up\n"
      "                     order 'P' for P=0, 1, 2, ....\n"
      "                     ** At most one 'detrend' filter can be used!\n"
      "                     ** You can use both '-adetrend' and '-detrend',\n"
      "                        but I don't know why you would try this.\n"
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
         ADD_FILTER(rank_order_float) ;             /* external function */
         INFO_message("Filter #%d = rank",nffunc) ;

       } else if( strncasecmp(argv[nopt],"adaptive:",9) == 0 ){
         char *cpt=argv[nopt]+9 ;
         if( hwid > 0 )
           ERROR_exit("You can't use more than one 'adaptive' filter :(") ;
         if( !isdigit(*cpt) )
           ERROR_exit("'%s' is not a valid 'adaptive' filter name",argv[nopt]) ;
         hwid = (int)strtod(cpt,NULL) ;
         if( hwid > 29 )
           WARNING_message("Very long filter '%s' will be very slow",argv[nopt]) ;
         else if( hwid <= 0 )
           ERROR_exit("'%s' is not a legal 'adaptive' filter name",argv[nopt]) ;
         ADD_FILTER(FILTER_adaptive) ; dosub = 0 ;
         INFO_message("Filter #%d = adaptive:%d",nffunc,hwid) ;

       } else if( strncasecmp(argv[nopt],"adetrend:",9) == 0 ){
         char *cpt=argv[nopt]+9 ;
         if( hwid > 0 )
           ERROR_exit("You can't use more than one 'adaptive' filter :(") ;
         if( !isdigit(*cpt) )
           ERROR_exit("'%s' is not a valid 'adetrend' filter name",argv[nopt]) ;
         hwid = (int)strtod(cpt,NULL) ;
         if( hwid > 29 )
           WARNING_message("Very long filter '%s' will be very slow",argv[nopt]) ;
         else if( hwid <= 0 )
           ERROR_exit("'%s' is not a legal 'adaptive' filter name",argv[nopt]) ;
         ADD_FILTER(FILTER_adaptive) ; dosub = 1 ;
         INFO_message("Filter #%d = adetrend:%d",nffunc,hwid) ;

       } else if( strncasecmp(argv[nopt],"detrend:",8) == 0 ){
         char *cpt=argv[nopt]+8 ;
         if( polort > 0 )
           ERROR_exit("You can't use more than one 'detrend' filter :(") ;
         if( !isdigit(*cpt) )
           ERROR_exit("'%s' is not a valid 'detrend' filter name",argv[nopt]) ;
         polort = (int)strtod(cpt,NULL) ;
         if( polort < 0 )
           ERROR_exit("'%s' is not a legal 'detrend' filter name",argv[nopt]) ;
         ADD_FILTER(FILTER_polort) ;
         INFO_message("Filter #%d = detrend:%d",nffunc,polort) ;

       } else if( strcasecmp(argv[nopt],"despike") == 0 ){
         if( nHH_despike > 0 )
           ERROR_exit("You can't use more than one 'despike' filter :(") ;
         ADD_FILTER(FILTER_despikeHH) ; nHH_despike = 12 ;
         INFO_message("Filter #%d = despike:12",nffunc) ;

       } else if( strncasecmp(argv[nopt],"despike:",8) == 0 ){
         char *cpt=argv[nopt]+8 ;
         if( nHH_despike > 0 )
           ERROR_exit("You can't use more than one 'despike' filter :(") ;
         if( !isdigit(*cpt) )
           ERROR_exit("'%s' is not a valid 'despike' filter name",argv[nopt]) ;
         nHH_despike = (int)strtod(cpt,NULL) ;
         if( nHH_despike < 4 || nHH_despike > 666 )
           ERROR_exit("'%s' is not a legal 'despike' filter name",argv[nopt]) ;
         if( nHH_despike > 39 )
           WARNING_message("Very long filter '%s' will be very slow",argv[nopt]) ;
         ADD_FILTER(FILTER_despikeHH) ;
         INFO_message("Filter #%d = despike:%d",nffunc,nHH_despike) ;

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
   if( nvals < 9 )
     ERROR_exit("Input dataset too short to filter: nvals=%d < 9",nvals) ;

   if( 4*hwid+1 > nvals )
     ERROR_exit(
       "adaptive filter half-width %d is too big for data length %d",
       hwid,nvals) ;

   if( 4*nHH_despike > nvals )
     ERROR_exit(
       "despike filter half-width %d is too big for data length %d",
       nHH_despike,nvals) ;

   if( hwid > 0 ) setup_adaptive_filter( hwid , nvals ) ;

   INFO_message("Load input dataset") ;

   DSET_load(old_dset) ; CHECK_LOAD_ERROR(old_dset) ;

   /** do the work **/

   fprintf(stderr,"++ Voxel processing: ") ;
   vstep = DSET_NVOX(old_dset) / 50 ;

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

   fprintf(stderr,"!\n") ;

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
