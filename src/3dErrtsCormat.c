#include "mrilib.h"

/*-------------------------------------------------------------------*/

#undef  FREEIF
#define FREEIF(p) do{ if((p)!=NULL){free(p);(p)=NULL;} }while(0)

static int maxlag = 0 ;
static float *cor = NULL ;
static int  *ncor = NULL ;

void CORMAT_init( int mlag )
{
   FREEIF(cor) ; FREEIF(ncor) ; maxlag = 0 ;
   if( mlag > 0 ){
      cor = (float *)calloc( sizeof(float) , mlag ) ;
     ncor = (int *)  calloc( sizeof(int)   , mlag ) ;
     maxlag = mlag ;
   }
   return ;
}

/*....................................................................*/

MRI_IMAGE * CORMAT_fetch(void)
{
   MRI_IMAGE *cim ; float *car ; int ii ;
   if( maxlag <= 0 || cor == NULL ) return NULL ;
   cim = mri_new(maxlag,1,MRI_float) ; car = MRI_FLOAT_PTR(cim) ;
   for( ii=0 ; ii < maxlag ; ii++ )
     if( ncor[ii] > 0 ) car[ii] = cor[ii] / ncor[ii] ;
   return cim ;
}

/*....................................................................*/

void CORMAT_add_vector( int nv , float *vv )
{
   int ii,itop , ktop,kk ;
   float ss , sq ;

   if( maxlag <= 0 || nv <= 2 || vv == NULL ) return ;

   ss = 0.0f ;
   for( ii=0 ; ii < nv ; ii++ ) ss += vv[ii] ;
   ss /= nv ; sq = 0.0f ;
   for( ii=0 ; ii < nv ; ii++ ) sq += (vv[ii]-ss)*(vv[ii]-ss) ;
   if( sq == 0.0f ) return ;
   sq = 1.0f / sq ;
   ktop = MIN(maxlag,nv-1) ;
   for( kk=1 ; kk <= ktop ; kk++ ){
     itop = nv-kk ;
     for( ii=0 ; ii < itop ; ii++ ){
       cor[kk-1] += (vv[ii]-ss)*(vv[ii+kk]-ss)*sq; ncor[kk-1]++;
     }
   }

   return ;
}

/*-------------------------------------------------------------------*/

void Syntax(void)
{
   printf(
    "Usage: 3dErrtsCormat [options] dset\n"
    "Computes the correlation (not covariance) matrix corresponding\n"
    "to the residual (or error) time series in 'dset', which will\n"
    "usually be the '-errts' output from 3dDeconvolve.  The output\n"
    "is a 1D file of the Toeplitz entries (to stdout).\n"
    "\n"
    "Options:\n"
    "  -concat rname\n"
    "\n"
    "-- RWCox -- Jun 2008\n"
   ) ;
   exit(0) ;
}

/*-------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   MRI_IMAGE *concim=NULL, *corim ; float *concar, *corar ;
   THD_3dim_dataset *inset=NULL ;
   int iarg , ii,nvox ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ) Syntax() ;

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-concat") == 0 ){
       if( concim != NULL )
         ERROR_exit("Can't have two %s options!",argv[iarg]) ;
       if( ++iarg >= argv )
         ERROR_exit("Need argument after option %s",argv[iarg-1]) ;
       concim = mri_read_1D( argv[iarg] ) ;
       if( concim == NULL )
         ERROR_exit("Can't read -concat file '%s'",argv[iarg]) ;
       if( concim->nx < 2 )
         ERROR_exit("-concat file '%s' must have at least 2 entries!",
                    argv[iarg]) ;
       concar = MRI_FLOAT_PTR(concim) ;
       for( ii=1 ; ii < concim->nx ; ii++ )
         if( (int)concar[ii-1] >= (int)concar[ii] )
           ERROR_exit("-concat file '%s' is not ordered increasingly!",
                      argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL )
         ERROR_exit("Can't have two %s options!",argv[iarg]) ;
       if( ++iarg >= argv )
         ERROR_exit("Need argument after option %s",argv[iarg-1]) ;
       inset = THD_open_dataset( argv[iarg] ) ;
       if( inset == NULL )
         ERROR_exit("Can't open -input dataset '%s'",argv[iarg]) ;
       if( DSET_NVALS(inset) < 9 )
         ERROR_exit("Dataset '%s' has only %d time points",
                    argv[iarg],DSET_NVALS(inset) ) ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   if( iarg == argc && inset == NULL )
     ERROR_exit("No input dataset on command line?") ;

   if( inset == NULL ){
     inset = THD_open_dataset( argv[iarg] ) ;
     if( inset == NULL )
       ERROR_exit("Can't open -input dataset '%s'",argv[iarg]) ;
     if( DSET_NVALS(inset) < 9 )
       ERROR_exit("Dataset '%s' has only %d time points",
                  argv[iarg],DSET_NVALS(inset) ) ;
   }

   DSET_load(inset); CHECK_LOAD_ERROR(inset);

}
