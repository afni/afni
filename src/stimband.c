#include "mrilib.h"

/*----------------------------------------------------------------------------*/

float_pair power_band( int nt , float *tar , float tr )
{
  float_pair pb={0.0f,0.0f} ;
  int ii , nfft , ntop , ibot,itop ;
  complex *cxar ; float *aar ; int *iar ; float df,asum,bsum ;

  nfft = csfft_nextup_even(nt) ; ntop = nfft/2-1 ; df = 1.0f/(tr*nfft) ;
  cxar = (complex *)calloc(sizeof(complex),nfft) ;
  aar  = (float   *)calloc(sizeof(float  ),nfft) ;
  iar  = (int     *)calloc(sizeof(int    ),nfft) ;
  for( ii=0 ; ii < nt ; ii++ ) aar[ii] = tar[ii] ;
  THD_quadratic_detrend( nt , aar , NULL,NULL,NULL) ;
  for( ii=0 ; ii < nt ; ii++ ) cxar[ii].r = aar[ii] ;
  csfft_cox( -1 , nfft , cxar ) ;
  for( ii=0 ; ii < ntop ; ii++ ){
    aar[ii] = CSQR(cxar[ii+1]) ; iar[ii] = ii+1 ;
  }
  qsort_floatint( ntop , aar , iar ) ;
  for( asum=ii=0 ; ii < ntop ; ii++ ) asum += aar[ii] ;
  if( asum <= 0.0f ){
    free(iar); free(aar); free(cxar); return pb;
  }
  asum *= 0.90f ;
  for( bsum=0,ii=ntop-1 ; ii >= 0 && bsum < asum ; ii-- ) bsum += aar[ii] ;
  ibot = iar[ii+1] ; itop = iar[ntop-1] ;
  if( ibot > itop ){ ii = ibot; ibot = itop; itop = ii; }
  pb.a = (ibot-0.5f)*df ; pb.b = (itop+0.5f)*df ;

  free(iar); free(aar); free(cxar);
  return pb ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 ;
   int          nmat=0 ;
   NI_element **nelmat=NULL , *nel ;
   char       **matnam=NULL ;
   float min_bwidth=0.04 ;
   float tr=1.0f ; int nt,nstim ; int *stim_bot=NULL,*stim_top=NULL ;
   float *col ;
   int mm,ss,ii ; char *cgl ; NI_int_array *giar ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
            "Usage: stimband [options] matrixfile ...\n"
     ) ;
     exit(0) ;
   }

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-matrix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       for( ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
         nel = NI_read_element_fromfile( argv[iarg] ) ;
         if( nel == NULL || nel->type != NI_ELEMENT_TYPE )
           ERROR_exit("Can't read matrix from file %s",argv[iarg]) ;
         nelmat = (NI_element **)realloc(nelmat,sizeof(NI_element *)*(nmat+1)) ;
         matnam = (char       **)realloc(matnam,sizeof(char       *)*(nmat+1)) ;
         nelmat[nmat] = nel ;
         matnam[nmat] = strdup(argv[iarg]) ;
         nmat++ ;
       }
       continue ;
     }

     if( strcasecmp(argv[iarg],"-min_bwidth") == 0 ){
       float val ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       val = (float)strtod(argv[iarg],NULL) ;
       if( val <= 0.0f || val > 0.20f )
         ERROR_message("-min_bwidth %g is out of range -- ignoring",val) ;
       else
         min_bwidth = val ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option %s",argv[iarg]) ;
   }

   /* any args left are matrix filenames */

   for( ; iarg < argc ; iarg++ ){
     nel = NI_read_element_fromfile( argv[iarg] ) ;
     if( nel == NULL || nel->type != NI_ELEMENT_TYPE )
       ERROR_exit("Can't read matrix from file %s",argv[iarg]) ;
     nelmat = (NI_element **)realloc(nelmat,sizeof(NI_element *)*(nmat+1)) ;
     matnam = (char       **)realloc(matnam,sizeof(char       *)*(nmat+1)) ;
     nelmat[nmat] = nel ;
     matnam[nmat] = strdup(argv[iarg]) ;
     nmat++ ;
   }

   if( nmat <= 0 )
     ERROR_exit("No matrices given for input?! :-(") ;

   for( mm=0 ; mm < nmat ; mm++ ){
     nel = nelmat[mm] ;
     cgl = NI_get_attribute( nel , "NRowFull" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix %s is missing 'NRowFull' attribute!",matnam[mm]) ;
     nt = (int)strtod(cgl,NULL) ;
     if( nt < 16 ) ERROR_exit("Matrix %s has NRowFull=%d -- too small to use!",matnam[mm],nt) ;

     cgl = NI_get_attribute( nel , "Nstim" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix %s is missing 'NStim' attribute!",matnam[mm]) ;
     nstim = (int)strtod(cgl,NULL) ;
     if( nstim <= 0 ) ERROR_exit("Matrix %s has NStim=%d -- doesn't make sense!",matnam[mm],nstim) ;

     cgl = NI_get_attribute( nel , "RowTR") ;
     if( cgl == NULL ){
       WARNING_message("Matrix %s is missing 'RowTR' attribute! Assuming TR=1.",matnam[mm]) ;
       tr = 1.0f ;
     } else {
       tr = (float)strtod(cgl,NULL) ;
       if( tr <= 0.0f ) ERROR_exit("Matrix %s has RowTR=%g -- illegal value!",matnam[mm],tr) ;
     }

     cgl = NI_get_attribute( nel , "StimBots" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix %s is missing 'StimBots' attribute!",matnam[mm]) ;
     giar = NI_decode_int_list( cgl , ";," ) ;
     if( giar == NULL || giar->num < stim_num )
       ERROR_exit("Matrix %s attribute 'StimBots' badly formatted?!",matnam[mm]) ;
     stim_bot = giar->ar ;

     cgl = NI_get_attribute( nel , "StimTops" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix %s is missing 'StimTops' attribute!",matnam[mm]) ;
     giar = NI_decode_int_list( cgl , ";," ) ;
     if( giar == NULL || giar->num < stim_num )
       ERROR_exit("Matrix %s attribute 'StimTops' badly formatted?!",matnam[mm]) ;
     stim_top = giar->ar ;

