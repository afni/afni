#include "mrilib.h"

/*** bandpass functions: 30 Apr 2009 -- RWCox ***/

/*--------------------------------------------------------------------------*/

static int nfft_fixed = 0 ;

int THD_bandpass_set_nfft( int n )
{
  nfft_fixed = (n >= 16) ? csfft_nextup_one35(n) : 0 ;
  return nfft_fixed ;
}

/*--------------------------------------------------------------------------*/
/*! Check THD_bandpass_vectors() input parameters for OK-ness.
    Returns 1 if OK, 0 if not OK.  If verb!=0, prints an info message.
*//*------------------------------------------------------------------------*/

int THD_bandpass_OK( int nx , float dt , float fbot , float ftop , int verb )
{
   int nfft , jbot,jtop ; float df ;
   static int wrn=1;

   if( nx   <  9    ) return 0 ;
   if( dt   <= 0.0f ) dt   = 1.0f ;
   if( fbot <  0.0f ) fbot = 0.0f ;
   if( ftop <= fbot ){ ERROR_message("bad bandpass frequencies?"); return 0; }
   if( wrn && dt > 60 ){
     WARNING_message("Your bandpass timestep (%f) is high.\n"
                     "   Make sure units are 'sec', not 'msec'.\n"
                     "   This warning will not be repeated." ,
                     dt);
     wrn = 0;
   }

   nfft = (nfft_fixed >= nx) ? nfft_fixed : csfft_nextup_one35(nx) ;
   df   = 1.0f / (nfft * dt) ;
   jbot = (int)rint(fbot/df) ;
   jtop = (int)rint(ftop/df) ;
   if( jtop >= nfft/2 ) jtop = nfft/2-1 ;
   if( jbot+1 >= jtop ){
     ERROR_message("bandpass: fbot and ftop too close ==> jbot=%d jtop=%d",jbot,jtop) ;
     return 0 ;
   }
   if( verb )
     ININFO_message(
       "bandpass: ntime=%d nFFT=%d dt=%.6g dFreq=%.6g Nyquist=%.6g passband indexes=%d..%d",
       nx, nfft, dt, df, (nfft/2)*df, jbot, jtop) ;
   return 1 ;
}

/*--------------------------------------------------------------------------*/
/*! Bandpass a set of vectors, optionally removing some orts as well.
   - Uses FFTs for the bandpass-ization, and least squares for the ort-ing.
   - To do a highpass only, set ftop to something larger than the Nyquist
     frequency (e.g., 999999.9).  To do a lowpass only, set fbot to 0.0.
   - However, the 0 and Nyquist frequencies are always removed.
   - Return value is the number of linear dimensions projected out.
     If 0 is returned, something bad happened.
*//*------------------------------------------------------------------------*/

int THD_bandpass_vectors( int nlen , int nvec   , float **vec ,
                          float dt , float fbot , float ftop  ,
                          int qdet , int nort   , float **ort  )
{
   int nfft,nby2 , iv, jbot,jtop , ndof=0 ; register int jj ;
   float df , tapr ;
   static int wrn = 1;
   register float *xar, *yar=NULL ;
   register complex *zar ; complex Zero={0.0f,0.0f} ;

ENTRY("THD_bandpass_vectors") ;

   if( nlen < 9 || nvec < 1 || vec == NULL ){
     ERROR_message("bad bandpass data?");
     RETURN(ndof);
   }
   if( wrn && dt > 60.0f ) {
     WARNING_message("Your bandpass timestep (%f) is high.\n"
                     "   Make sure units are 'sec', not 'msec'.\n"
                     "   This warning will not be repeated." ,
                     dt);
     wrn = 0;
   }
   if( dt   <= 0.0f ) dt   = 1.0f ;
   if( fbot <  0.0f ) fbot = 0.0f ;
   if( ftop <= fbot ){
     ERROR_message("Bad bandpass frequencies?"); RETURN(ndof);
   }
   if( nort >= nlen ){
     ERROR_message("Too many bandpass orts?")  ; RETURN(ndof);
   }

   /** setup for FFT **/

   nfft = (nfft_fixed >= nlen) ? nfft_fixed : csfft_nextup_one35(nlen) ;
   nby2 = nfft/2 ;

   df   = 1.0f / (nfft * dt) ;           /* frequency resolution */
   jbot = (int)rint(fbot/df) ;           /* closest freq index to fbot */
   jtop = (int)rint(ftop/df) ;           /* and to ftop */
   if( jtop >= nby2   ) jtop = nby2-1 ;  /* can't go past Nyquist! */
   if( jbot >= jtop+1 ){
     ERROR_message("bandpass: fbot and ftop too close ==> "
                   "jbot=%d jtop=%d (df=%f)",
                   jbot,jtop, df) ;
     RETURN(ndof) ;
   }

   /** quadratic detrending first? (should normally be used) **/

   if( qdet ){
     ndof += 2 ;
     for( iv=0 ; iv < nvec ; iv++ )
       THD_quadratic_detrend( nlen, vec[iv], NULL,NULL,NULL ) ;
   }

   zar = (complex *)malloc(sizeof(complex)*nfft) ;  /* work array */
   csfft_scale_inverse(1) ;                         /* scale inverse FFT by 1/nfft */

   /** loop over vectors in pairs, FFT-ing and bandpassing **/

   ndof += 2 ;  /* for 0 and Nyquist freqs */

   if( jbot >= 1 ) ndof += 2*jbot - 1 ;  /* DOF for low freq */
   ndof += 2*(nby2-jtop) - 1 ;           /* DOF for high freq */

   for( iv=0 ; iv < nvec ; iv+=2 ){

     /* load a pair of vectors into zar to double up on FFTs of real data */

     xar = vec[iv] ;
     if( iv == nvec-1 ){  /* last one has nothing to pair with */
       for( jj=0 ; jj < nlen ; jj++ ){ zar[jj].r = xar[jj] ; zar[jj].i = 0.0f ; }
     } else {
       yar = vec[iv+1] ;
       for( jj=0 ; jj < nlen ; jj++ ){ zar[jj].r = xar[jj] ; zar[jj].i = yar[jj] ; }
     }
     for( jj=nlen ; jj < nfft ; jj++ ) zar[jj] = Zero ;  /* zero fill */

     csfft_cox( -1 , nfft , zar ) ;  /*** the FFT ***/

     /* delete unwanted frequencies */

     zar[0] = zar[nby2] = Zero ;

     tapr = (nort > 0 && ort != NULL) ? 0.05f : 0.5f ;

     if( jbot >= 1 ){
       zar[jbot].r      *= tapr ; zar[jbot].i      *= tapr ;
       zar[nfft-jbot].r *= tapr ; zar[nfft-jbot].i *= tapr ;
       for( jj=1 ; jj < jbot ; jj++ ) zar[jj] = zar[nfft-jj] = Zero ;
     }

     zar[jtop].r      *= tapr ; zar[jtop].i      *= tapr ;
     zar[nfft-jtop].r *= tapr ; zar[nfft-jtop].i *= tapr ;
     for( jj=jtop+1 ; jj < nby2 ; jj++ ) zar[jj] = zar[nfft-jj] = Zero ;

     csfft_cox( 1 , nfft , zar ) ;  /*** inverse FFT ***/

     /* unload vector pair back into original data arrays */

     if( iv == nvec-1 ){
       for( jj=0 ; jj < nlen ; jj++ ) xar[jj] = zar[jj].r ;
     } else {
       for( jj=0 ; jj < nlen ; jj++ ){ xar[jj] = zar[jj].r ; yar[jj] = zar[jj].i ; }
     }

   } /* end of loop over vector pairs: bandpassing now done */

   free(zar) ; csfft_scale_inverse(0) ;

   /*** remove orts? ***/

   if( nort > 0 && ort != NULL ){
     float **qort = (float **)malloc(sizeof(float *)*nort) ;
     MRI_IMAGE *qim , *pim ; float *par, *qar , *rar , *pt,*qt ;
     register float sum , xt ; register int kk ;

     /* must bandpass copy of orts first -- via recursion
        (so we don't re-introduce any of the removed frequencies) */

     qim = mri_new( nlen , nort , MRI_float ) ; /** [Q] = nlen X nort  **/
     qar = MRI_FLOAT_PTR(qim) ;                 /** load with the orts **/
     for( iv=0 ; iv < nort ; iv++ ){
       qort[iv] = qar + iv*nlen ;
       memcpy( qort[iv] , ort[iv] , sizeof(float)*nlen ) ;
     }

     (void)THD_bandpass_vectors( nlen, nort, qort, dt, fbot, ftop, qdet, 0,NULL ) ;
     free(qort) ;

     /* compute pseudo-inverse ([P] = inv{[Q]'[Q]}[Q]') of bandpassed orts */

     pim = mri_matrix_psinv( qim , NULL , 1.e-8 ) ;  /** [P] = nort X nlen **/
     if( pim == NULL ){  /* should not happen */
       mri_free(qim) ;
       ERROR_message("can't remove bandpass orts?") ;
       RETURN(ndof) ;
     }

     par = MRI_FLOAT_PTR(pim) ;  /* nort X nlen matrix */

     /* Project the bandpassed orts out of data vectors:
        for each vector [y], replace it with [y] - [Q][P][y] ;
        this is more efficient than computing the nlen X nlen projection
        matrix [I]-[Q][P] and then applying to each vector, since
        that would require about nlen*nlen flops per vector, whereas
        the 2 matrix multiplications by [P] then [Q] will require
        about 2*nlen*nort flops per vector, a clear win if nort < nlen/2. */

     rar = (float *)malloc(sizeof(float)*nort) ;  /* will hold [P][y] */
     for( iv=0 ; iv < nvec ; iv++ ){
       xar = vec[iv] ;  /* [y] vector */

       /* compute nort-vector [r] = [P][y] */

       for( jj=0 ; jj < nort ; jj++ ) rar[jj] = 0.0f ;  /* initialize to 0 */
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

     ndof += nort ;

   } /* de-ortification is done */

   /** done **/

   if( nfft > nlen ){
     double fac = ((double)nlen)/(double)nfft ;
     ndof = (int)rint(fac*ndof) ;
   }
   RETURN(ndof) ;
}

/*--------------------------------------------------------------------------*/
/*! Bandpass a vectim. */

int THD_bandpass_vectim( MRI_vectim *mrv ,
                         float dt , float fbot , float ftop  ,
                         int qdet , int nort   , float **ort  )
{
   float **vec ; int nlen , nvec , ndof , kk ;

ENTRY("THD_bandpass_vectim") ;

   if( mrv == NULL ) RETURN(0) ;

   nvec = mrv->nvec ; nlen = mrv->nvals ;
   vec  = (float **)malloc(sizeof(float *)*nvec) ;
   for( kk=0 ; kk < nvec ; kk++ ) vec[kk] = VECTIM_PTR(mrv,kk) ;

   ndof = THD_bandpass_vectors( nlen , nvec , vec ,
                                dt , fbot , ftop  , qdet , nort , ort ) ;

   free(vec) ; RETURN(ndof) ;
}
