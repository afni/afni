#include "mrilib.h"

/*-------------------------------------------------------------------*/

#undef  FREEIF
#define FREEIF(p) do{ if((p)!=NULL){free(p);(p)=NULL;} }while(0)

static int maxlag = 0 ;
static float *cor = NULL ;
static int  *ncor = NULL ;
static int   port = 0 ;

/*....................................................................*/

void CORMAT_init( int mlag , int pp )
{
   FREEIF(cor) ; FREEIF(ncor) ; maxlag = 0 ;
   if( mlag > 0 ){
      cor   = (float *)calloc( sizeof(float) , mlag ) ;
     ncor   = (int *)  calloc( sizeof(int)   , mlag ) ;
     maxlag = mlag ;
     port   = (pp < 0 || pp > 3) ? 0 : pp ;
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

   switch(port){
     default:
     case 0:  THD_const_detrend      ( nv , vv , NULL           ); break;
     case 1:  THD_linear_detrend     ( nv , vv , NULL,NULL      ); break;
     case 2:  THD_quadratic_detrend  ( nv , vv , NULL,NULL,NULL ); break;
     case 3:  THD_cubic_detrend      ( nv , vv                  ); break;
   }
   THD_normRMS( nv , vv ) ;

   ktop = MIN(maxlag,nv-1) ;
   for( kk=1 ; kk <= ktop ; kk++ ){
     itop = nv-kk ;
     for( ii=0 ; ii < itop ; ii++ ){
       cor[kk-1] += vv[ii] * vv[ii+kk] ; ncor[kk-1]++ ;
     }
   }

   return ;
}

/*-------------------------------------------------------------------*/

void Syntax(void)
{
   printf(
    "Usage: 3dErrtsCormat [options] dset\n"
    "\n"
    "Computes the correlation (not covariance) matrix corresponding\n"
    "to the residual (or error) time series in 'dset', which will\n"
    "usually be the '-errts' output from 3dDeconvolve.  The output\n"
    "is a 1D file of the Toeplitz entries (to stdout).\n"
    "\n"
    "Options:\n"
    "  -concat rname  = as in 3dDeconvolve\n"
    "  -input  dset   = alternate way of telling what dataset to read\n"
    "  -mask   mset   = mask dataset\n"
    "  -maxlag mm     = set maximum lag\n"
    "  -polort pp     = set polort level (default=0)\n"
    "\n"
    "-- RWCox -- June 2008 -- for my own pleasant purposes\n"
    "-- Also see program 3dLocalCormat to do this on each voxel,\n"
    "   and optionally estimate the ARMA(1,1) model parameters.\n"
   ) ;
   PRINT_COMPILE_DATE ; exit(0) ;
}

/*-------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   MRI_IMAGE *concim=NULL, *corim ;
   float     *concar=NULL     , *corar ;
   THD_3dim_dataset *inset=NULL , *mset=NULL ;
   int iarg , ii,jj,nvox,ntime , nbk,*bk,mlag , mmlag=0,pport=0 ;
   byte *mmm ; float *tar ;

   /*-- basic startup stuff --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ) Syntax() ;

   mainENTRY("3dErrtsCormat") ; machdep() ;
   PRINT_VERSION("3dErrtsCormat") ; AUTHOR("Zhark the Correlator") ;
   AFNI_logger("3dErrtsCormat",argc,argv) ;

   /*-- process command line options --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-polort") == 0 ){
       char *qpt ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after option %s",argv[iarg-1]) ;
       pport = (int)strtod(argv[iarg],&qpt) ;
       if( *qpt != '\0' ) WARNING_message("Illegal non-numeric value after -polort") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-maxlag") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after option %s",argv[iarg-1]) ;
       mmlag = (int)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-concat") == 0 ){
       if( concim != NULL )
         ERROR_exit("Can't have two %s options!",argv[iarg]) ;
       if( ++iarg >= argc )
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
       if( ++iarg >= argc )
         ERROR_exit("Need argument after option %s",argv[iarg-1]) ;
       inset = THD_open_dataset( argv[iarg] ) ;
       if( inset == NULL )
         ERROR_exit("Can't open -input dataset '%s'",argv[iarg]) ;
       if( DSET_NVALS(inset) < 9 )
         ERROR_exit("Dataset '%s' has only %d time points",
                    argv[iarg],DSET_NVALS(inset) ) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       if( mset != NULL )
         ERROR_exit("Can't have two %s options!",argv[iarg]) ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after option %s",argv[iarg-1]) ;
       mset = THD_open_dataset( argv[iarg] ) ;
       if( mset == NULL )
         ERROR_exit("Can't open -input dataset '%s'",argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   /*-- get input dataset --*/

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

   /*-- create byte mask array --*/

   nvox = DSET_NVOX(inset) ; ntime = DSET_NVALS(inset) ;

   if( mset != NULL ){
     if( !EQUIV_GRIDS(inset,mset) )
       ERROR_exit("Input and mask datasets don't have the same 3D grid!") ;
     mmm = THD_makemask( mset , 0 , 1.0f,-1.0f ) ;
     INFO_message("%d voxels in mask (out of %d total)",
                  THD_countmask(nvox,mmm),nvox) ;
     DSET_unload( mset ) ;
   } else {
     mmm = (byte *)malloc(sizeof(byte)*nvox) ;
     memset( mmm , 1 , sizeof(byte)*nvox ) ;
     INFO_message("Using all %d voxels in the dataset",nvox) ;
   }

   /*-- set up blocks of continuous time data --*/

   if( DSET_IS_TCAT(inset) ){
     if( concim != NULL ){
       WARNING_message("Ignoring -concat, since dataset is auto-catenated") ;
       mri_free(concim) ;
     }
     concim = mri_new(inset->tcat_num,1,MRI_float) ;
     concar = MRI_FLOAT_PTR(concim) ;
     concar[0] = 0.0 ;
     for( ii=0 ; ii < inset->tcat_num-1 ; ii++ )
       concar[ii+1] = concar[ii] + inset->tcat_len[ii] ;
   } else if( concim == NULL ){
     concim = mri_new(1,1,MRI_float) ;
     concar = MRI_FLOAT_PTR(concim)  ; concar[0] = 0 ;
   }
   nbk = concim->nx ;
   bk  = (int *)malloc(sizeof(int)*(nbk+1)) ;
   for( ii=0 ; ii < nbk ; ii++ ) bk[ii] = (int)concar[ii] ;
   bk[nbk] = ntime ;
   mri_free(concim) ;
   mlag = DSET_NVALS(inset) ;
   for( ii=0 ; ii < nbk ; ii++ ){
     jj = bk[ii+1]-bk[ii] ; if( jj < mlag ) mlag = jj ;
     if( bk[ii] < 0 || jj < 9 )
       ERROR_exit("something is rotten in the dataset run lengths") ;
   }
   mlag-- ;
   if( mmlag > 0 && mlag > mmlag ) mlag = mmlag ;
   else                            INFO_message("Max lag set to %d",mlag) ;

   /*-- do some work --*/

   CORMAT_init(mlag,pport) ;
   tar = (float *)malloc(sizeof(float)*ntime) ;

   for( jj=0 ; jj < nvox ; jj++ ){
     if( !mmm[jj] ) continue ;
     ii = THD_extract_array( jj , inset , 0 , tar ) ;
     if( ii < 0 ){ ERROR_message("bad data at voxel index #%d?",jj); continue; }
     for( ii=0 ; ii < nbk ; ii++ )
       CORMAT_add_vector( bk[ii+1]-bk[ii] , tar + bk[ii] ) ;
   }

   /*-- finish up --*/

   DSET_unload(inset) ;
   corim = CORMAT_fetch() ;
   if( corim == NULL ) ERROR_exit("CORMAT_fetch failed!?") ;
   corar = MRI_FLOAT_PTR(corim) ;
   printf(" 1.0\n") ;
   for( ii=0 ; ii < corim->nx ; ii++ ) printf(" %.5f\n",corar[ii]) ;

   exit(0) ;
}
