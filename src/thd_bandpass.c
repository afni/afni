#include "mrilib.h"

/*** bandpass functions: 30 Apr 2009 -- RWCox ***/

/*--------------------------------------------------------------------------*/

int THD_bandpass_OK( int nx , float dt , float fbot , float ftop )
{
   int nfft , jbot,jtop ; float df ;

   if( nx <  9    ) return 0 ;
   if( dt <= 0.0f ) dt = 1.0f ;
   if( fbot <  0.0f ) fbot = 0.0f ;
   if( ftop <= fbot ){ ERROR_message("bad bandpass frequencies?"); return 0; }

   nfft = csfft_nextup_one35(nx) ;
   df   = 1.0f / (nfft * dt) ;
   jbot = (int)rint(fbot/df) ;
   jtop = (int)rint(ftop/df) ;
   if( jbot+1 >= jtop ){
     ERROR_message("bandpass: fbot and ftop too close ==> jbot=%d jtop=%d",jbot,jtop) ;
     return 0 ;
   }
   INFO_message("nfft=%d df=%.4g passband indexes=%d..%d",nfft,df,jbot,jtop) ;
   return 1 ;
}

/*--------------------------------------------------------------------------*/
/*! Bandpass a set of vectors, optionally removing some orts as well.
    Uses FFTs for the bandpass-ization, and least squares for the ort-ing.
*//*------------------------------------------------------------------------*/

void THD_bandpass_vectors( int nlen , int nvec , float **vec ,
                           float dt , float fbot , float ftop ,
                           int qdet , int nort , float **ort )
{
   int nfft,nby2 , iv, jbot,jtop ; register int jj ;
   float df ;
   register float *xar, *yar=NULL ;
   register complex *zar ; complex Zero={0.0f,0.0f} ;

   if( nlen < 9 || nvec < 1 || vec == NULL ){ ERROR_message("bad bandpass data?"); return; }
   if( dt   <= 0.0f ) dt = 1.0f ;
   if( fbot <  0.0f ) fbot = 0.0f ;
   if( ftop <= fbot ){ ERROR_message("bad bandpass frequencies?"); return; }
   if( nort >= nlen ){ ERROR_message("too many bandpass orts?"); return; }

   /** quadratic detrending first? **/

   if( qdet ){
     for( iv=0 ; iv < nvec ; iv++ )
       THD_quadratic_detrend( nlen, vec[iv], NULL,NULL,NULL ) ;
   }

   /** setup for FFT **/

   nfft = csfft_nextup_one35( nlen ) ; nby2 = nfft/2 ;

   df   = 1.0f / (nfft * dt) ;
   jbot = (int)rint(fbot/df) ;
   jtop = (int)rint(ftop/df) ;

   zar = (complex *)malloc(sizeof(complex)*nfft) ;
   csfft_scale_inverse(1) ;

   /** loop over vectors in pairs, FFT-ing and bandpassing **/

   for( iv=0 ; iv < nvec ; iv+=2 ){

     xar = vec[iv] ;
     if( iv == nvec-1 ){
       for( jj=0 ; jj < nlen ; jj++ ){ zar[jj].r = xar[jj] ; zar[jj].i = 0.0f ; }
     } else {
       yar = vec[iv+1] ;
       for( jj=0 ; jj < nlen ; jj++ ){ zar[jj].r = xar[jj] ; zar[jj].i = yar[jj] ; }
     }
     for( jj=nlen ; jj < nfft ; jj++ ) zar[jj] = Zero ;

     csfft_cox( -1 , nfft , zar ) ;

     zar[0] = zar[nby2] = Zero ;

     if( jbot >= 1 ){
       zar[jbot].r      *= 0.5f ; zar[jbot].i      *= 0.5f ;
       zar[nfft-jbot].r *= 0.5f ; zar[nfft-jbot].i *= 0.5f ;
       for( jj=1 ; jj < jbot ; jj++ ) zar[jj] = zar[nfft-jj] = Zero ;
     }

     zar[jtop].r      *= 0.5f ; zar[jtop].i      *= 0.5f ;
     zar[nfft-jtop].r *= 0.5f ; zar[nfft-jtop].i *= 0.5f ;
     for( jj=jtop+1 ; jj < nby2 ; jj++ ) zar[jj] = zar[nfft-jj] = Zero ;

     csfft_cox( 1 , nfft , zar ) ;

     if( iv == nvec-1 ){
       for( jj=0 ; jj < nlen ; jj++ ) xar[jj] = zar[jj].r ;
     } else {
       for( jj=0 ; jj < nlen ; jj++ ){ xar[jj] = zar[jj].r ; yar[jj] = zar[jj].i ; }
     }

   }

   free(zar) ; csfft_scale_inverse(0) ;

   /*** remove orts? ***/

   if( nort > 0 && ort != NULL ){
     float **qort = (float **)malloc(sizeof(float *)*nort) ;
     MRI_IMAGE *qim , *pim ; float *par, *qar , *rar , *pt,*qt ;
     register float sum , xt ; register int kk ;

     /* bandpass copy of orts first */

     qim = mri_new( nlen , nort , MRI_float ) ;  /** call this [Q] = nlen X nort **/
     qar = MRI_FLOAT_PTR(qim) ;
     for( iv=0 ; iv < nort ; iv++ ){
       qort[iv] = qar + iv*nlen ;
       memcpy( qort[iv] , ort[iv] , sizeof(float)*nlen ) ;
     }
     THD_bandpass_vectors( nlen , nort , qort , dt , fbot , ftop , qdet , 0,NULL ) ;
     free(qort) ;

     /* compute pseudo-inverse ([P] = inv{[Q]'[Q]}[Q]') of bandpassed orts */

     pim = mri_matrix_psinv( qim , NULL , 1.e-8 ) ;  /** call this [P] = nort X nlen **/
     if( pim == NULL ){ mri_free(qim) ; ERROR_message("can't remove bandpass orts?"); return; }
     par = MRI_FLOAT_PTR(pim) ;  /* nort X nlen matrix */

     /* project bandpassed orts out of data vectors:
        for each vector [y], replace it with [y] - [Q][P][y] */

     rar = (float *)malloc(sizeof(float)*nort) ;  /* will hold [P][y] */
     for( iv=0 ; iv < nvec ; iv++ ){
       xar = vec[iv] ;

       /* compute nort-vector [r] = [P][y] */

       for( jj=0 ; jj < nort ; jj++ ) rar[jj] = 0.0f ;
       for( kk=0 ; kk < nlen ; kk++ ){
         pt = par + kk*nort ; xt = xar[kk] ;
         for( jj=0 ; jj < nort ; jj++ ) rar[jj] += pt[jj]*xt ;
       }

       /* now subtract [Q][r] from [y] */

       for( jj=0 ; jj < nort ; jj++ ){
         qt = qar + jj*nlen ; xt = rar[jj] ;
         for( kk=0 ; kk < nlen ; kk++ ) xar[kk] -= qt[kk]*xt ;
       }
     }

     free(rar) ; mri_free(pim) ; mri_free(qim) ;
   }

   /** done **/

   return ;
}
