#include "mrilib.h"

/*----------------------------------------------------------------------------*/

static float min_pow = 0.90f ;

float_pair power_band( int nt , float *tar , float tr )
{
  float_pair pb={0.0f,0.0f} ;
  int ii , nfft , ntop , ibot,itop ;
  complex *cxar ; float *aar ; int *iar ; float df,asum,bsum ;

ENTRY("power_band") ;

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
    free(iar); free(aar); free(cxar); RETURN(pb);
  }
  asum *= min_pow ;
  for( bsum=0,ii=ntop-1 ; ii >= 0 && bsum < asum ; ii-- ) bsum += aar[ii] ;
  ibot = iar[ii+1] ; itop = iar[ntop-1] ;
  if( ibot > itop ){ ii = ibot; ibot = itop; itop = ii; }
  pb.a = (ibot-0.5f)*df ; pb.b = (itop+0.5f)*df ;

  free(iar); free(aar); free(cxar);
  RETURN(pb) ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , verb=0 ;
   int          nmat=0 ;
   NI_element **nelmat=NULL , *nel ;
   char       **matnam=NULL ;
   float min_bwidth=0.03f ;
   float min_freq  =0.01f ;
   float tr=1.0f ; int nt,nstim ; int *stim_bot=NULL,*stim_top=NULL ;
   float *col=NULL ;
   int mm,ss,ii,kk ; char *cgl ; NI_int_array *giar ;
   float_pair pbs ; float pbot=1.e+22f,ptop=-1.e+22f , ppp ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
      "\n"
      "Usage: stimband [options] matrixfile ...\n"
      "\n"
      "The purpose of this program is to give a frequency band\n"
      "that covers at least 90%% of the 'power' (|FFT|^2) of the\n"
      "stimulus columns taken from one or more X.nocensor.xmat.1D\n"
      "files output by 3dDeconvolve.  The band (2 frequencies\n"
      "in Hertz) are printed to stdout.  This program is meant\n"
      "to be used in a script to decide on the passband for\n"
      "various pre- and post-processing steps in AFNI.\n"
      "\n"
      "If the output band is '0 0', this indicates that the input\n"
      "matrices did not have any valid columns marked as stimuli;\n"
      "this would be the case, for example, if the matrices had\n"
      "been generated solely for use in resting-state FMRI denoising.\n"
      "\n"
      "Options:\n"
      "--------\n"
      " -verb          = print (to stderr) the power band for each\n"
      "                  individual stimulus column from each matrix.\n"
      " -matrix mmm    = another way to read 1 or more matrix files.\n"
      " -min_freq aa   = set the minimum frequency output for the\n"
      "                  band to 'aa' [default value = 0.01].\n"
      " -min_bwidth bb = set the minimum bandwidth output (top frequency\n"
      "                  minus bottom frequency) to 'bb' [default = 0.03].\n"
      " -min_pow ff    = set the minimum power fraction to 'ff'%% instead\n"
      "                  of the default 90%%; ff must be in the range\n"
      "                  50..99 (inclusive).\n"
      "\n"
      "Quick Hack by RWCox, December 2015 -- Merry X and Happy New Y!\n"
     ) ;
     exit(0) ;
   }

   mainENTRY("stimband Main") ;

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

     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
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

     if( strcasecmp(argv[iarg],"-min_freq") == 0 ){
       float val ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       val = (float)strtod(argv[iarg],NULL) ;
       if( val <= 0.0f || val > 0.20f )
         ERROR_message("-min_freq %g is out of range -- ignoring",val) ;
       else
         min_freq = val ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-min_pow") == 0 ){
       float val ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       val = (float)strtod(argv[iarg],NULL) ;
       if( val < 50.0f || val > 99.0f )
         ERROR_message("-min_pow %g is out of range -- ignoring",val) ;
       else
         min_pow = 0.01f*val ;
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
     col = (float *)realloc(col,sizeof(float)*nt) ;

     cgl = NI_get_attribute( nel , "RowTR") ;
     if( cgl == NULL ){
       WARNING_message("Matrix %s is missing 'RowTR' attribute! Assuming TR=1.",matnam[mm]) ;
       tr = 1.0f ;
     } else {
       tr = (float)strtod(cgl,NULL) ;
       if( tr <= 0.0f ) ERROR_exit("Matrix %s has RowTR=%g -- illegal value!",matnam[mm],tr) ;
     }


     /* setup the columns to process, via attributes
          NStim, StimBots, StimTops (first choice)
        or via
          BasisNstim, BasisColumns (the backup procedure) */

     cgl = NI_get_attribute( nel , "Nstim" ) ;
     if( cgl != NULL ){
       nstim = (int)strtod(cgl,NULL) ;
       if( nstim <= 0 ) ERROR_exit("Matrix %s has NStim=%d -- doesn't make sense!",matnam[mm],nstim) ;

       cgl = NI_get_attribute( nel , "StimBots" ) ;
       if( cgl == NULL ) ERROR_exit("Matrix %s is missing 'StimBots' attribute!",matnam[mm]) ;
       giar = NI_decode_int_list( cgl , ";," ) ;
       if( giar == NULL || giar->num < nstim )
         ERROR_exit("Matrix %s attribute 'StimBots' badly formatted?!",matnam[mm]) ;
       stim_bot = giar->ar ;

       cgl = NI_get_attribute( nel , "StimTops" ) ;
       if( cgl == NULL ) ERROR_exit("Matrix %s is missing 'StimTops' attribute!",matnam[mm]) ;
       giar = NI_decode_int_list( cgl , ";," ) ;
       if( giar == NULL || giar->num < nstim )
         ERROR_exit("Matrix %s attribute 'StimTops' badly formatted?!",matnam[mm]) ;
       stim_top = giar->ar ;
     } else {
       char anam[256] ; int *sb , *st , nfail=0 ;
       cgl = NI_get_attribute( nel , "BasisNstim" ) ;
       if( cgl == NULL )
         ERROR_exit("Matrix %s is missing NStim *and* BasisNstim attributes?!",matnam[mm]) ;
       nstim = (int)strtod(cgl,NULL) ;
       if( nstim <= 0 ) ERROR_exit("Matrix %s has BasisNStim=%d -- doesn't make sense!",matnam[mm],nstim) ;
       sb = (int *)malloc(sizeof(int)*nstim) ;
       st = (int *)malloc(sizeof(int)*nstim) ;
       for( ss=0 ; ss < nstim ; ss++ ){
         sprintf(anam,"BasisColumns_%06d",ss+1) ;
         cgl = NI_get_attribute( nel , anam ) ;
         if( cgl == NULL ){
           sb[ss] = 666 ; st[ss] = -666 ; nfail++ ;
         } else {
           sscanf(cgl,"%d:%d",sb+ss,st+ss) ;
         }
       }
       if( nfail == nstim )
         ERROR_exit("Matrix %s is missing all info about which columns are stimuli!",matnam[mm]) ;
       stim_bot = sb ; stim_top = st ;
     }

     for( ss=0 ; ss < nstim ; ss++ ){
       for( kk=stim_bot[ss] ; kk <= stim_top[ss] ; kk++ ){
         if( nel->vec_typ[kk] == NI_FLOAT ){
           float *cd=(float *)nel->vec[kk] ;
           for( ii=0 ; ii < nt ; ii++ ) col[ii] = cd[ii] ;
         } else if( nel->vec_typ[kk] == NI_DOUBLE ){
           double *cd=(double *)nel->vec[kk] ;
           for( ii=0 ; ii < nt ; ii++ ) col[ii] = (float)cd[ii] ;
         } else {
           ERROR_exit("illegal data type in column %d of matrix %s",kk,matnam[mm]) ;
         }
         pbs = power_band( nt , col , tr ) ;
         if( verb )
           INFO_message("matrix=%s column=%d powerband=%g:%g",matnam[mm],kk,pbs.a,pbs.b) ;
         if( pbs.a < pbs.b ){
           if( pbot > pbs.a ) pbot = pbs.a ;
           if( ptop < pbs.b ) ptop = pbs.b ;
         }
       }
     }
   }

   if( pbot > ptop ){ printf(" 0 0\n") ; exit(0) ; }

   if( pbot < min_freq ) pbot = min_freq ;
   if( ptop-pbot < min_bwidth ){
     pbot -= 0.5f * ( min_bwidth - (ptop-pbot) ) ;
     if( pbot < min_freq ) pbot = min_freq ;
     ptop = pbot + min_bwidth ;
   }
   printf(" %g %g\n",pbot,ptop) ;
   exit(0) ;
}
