#include "mrilib.h"

/*** bandpass functions: 30 Apr 2009 -- RWCox ***/

/*--------------------------------------------------------------------------*/

static int bpwrn = 1 ;

static int nfft_fixed = 0 ;

int THD_bandpass_set_nfft( int n )
{
  nfft_fixed = (n >= 16) ? csfft_nextup_even(n) : 0 ;
  return nfft_fixed ;
}

/*--------------------------------------------------------------------------*/
/*! Check THD_bandpass_vectors() input parameters for OK-ness.
    Returns 1 if OK, 0 if not OK.  If verb!=0, prints an info message.
*//*------------------------------------------------------------------------*/

int THD_bandpass_OK( int nx , float dt , float fbot , float ftop , int verb )
{
   int nfft , jbot,jtop ; float df ;

   if( ftop > ICOR_MAX_FTOP ) return 1 ;  /* 26 Feb 2010 */

   if( nx   <  9    ) return 0 ;
   if( dt   <= 0.0f ) dt   = 1.0f ;
   if( fbot <  0.0f ) fbot = 0.0f ;
   if( ftop <= fbot ){ ERROR_message("bad bandpass frequencies?"); return 0; }
   if( bpwrn && dt > 60.0f ){
     WARNING_message("Your bandpass timestep (%f) is high.\n"
                     "   Make sure units are 'sec', not 'msec'.\n"
                     "   This warning will not be repeated." ,
                     dt);
     bpwrn = 0;
   }

   nfft = (nfft_fixed >= nx) ? nfft_fixed : csfft_nextup_even(nx) ;
   df   = 1.0f / (nfft * dt) ;  /* freq step */
   jbot = (int)rint(fbot/df) ;  /* band bot index */
   jtop = (int)rint(ftop/df) ;  /* band top index */
   if( jtop >= nfft/2 ) jtop = nfft/2-1 ;
   if( jbot+1 >= jtop ){
     ERROR_message(
       "bandpass: fbot=%g and ftop=%g too close ==> jbot=%d jtop=%d [nfft=%d dt=%g]",
       fbot,ftop,jbot,jtop,nfft,dt) ;
     return 0 ;
   }

   if( verb )
     ININFO_message(
       "bandpass: ntime=%d nFFT=%d dt=%.6g dFreq=%.6g Nyquist=%.6g passband indexes=%d..%d",
       nx, nfft, dt, df, (nfft/2)*df, jbot, jtop) ;

   return 1 ;
}

/*--------------------------------------------------------------------------*/
/*! Return the number of degrees of freedom that would remain after
    bandpassing (the dimension of the subspace the data will be 
    projected into).

    Returns twice the length of the passband index range.
    Returns 0 if life is bad.

    based on THD_bandpass_OK                    18 Mar 2015 [rickr]
*//*------------------------------------------------------------------------*/

int THD_bandpass_remain_dim(int nx, float dt, float fbot, float ftop, int verb)
{
   int nfft , jbot,jtop ; float df ;

   if( nx   <  9    ) {
      if( verb ) WARNING_message("length %d too short for bandpassing", nx);
      return 0 ; }

   if( dt   <= 0.0f ) dt   = 1.0f ;
   if( fbot <  0.0f ) fbot = 0.0f ;
   if( ftop <= fbot ){
      if( verb ) WARNING_message("bad bandpass frequencies (ftop<=fbot)");
      return 0; }
   if( verb && dt > 60.0f ){
     WARNING_message("Your bandpass timestep (%f) is high.\n"
                     "   Make sure units are 'sec', not 'msec'.\n"
                     "   This warning will not be repeated." ,
                     dt);
   }

   nfft = (nfft_fixed >= nx) ? nfft_fixed : csfft_nextup_even(nx) ;
   df   = 1.0f / (nfft * dt) ;  /* freq step */
   jbot = (int)rint(fbot/df) ;  /* band bot index */
   jtop = (int)rint(ftop/df) ;  /* band top index */
   if( jtop >= nfft/2 ) jtop = nfft/2-1 ;
   if( jbot+1 >= jtop ){
     if( verb )
        WARNING_message("bandpass: fbot=%g and ftop=%g too close"
                        " ==> jbot=%d jtop=%d [nfft=%d dt=%g]",
                        fbot,ftop,jbot,jtop,nfft,dt) ;
     return 0 ;
   }

   return 2*(jtop-jbot+1);
}

/*--------------------------------------------------------------------------*/
/*! Bandpass a set of vectors, optionally removing some orts as well.
   - Uses FFTs for the bandpass-ization, and least squares for the ort-ing.
   - To do a highpass only, set ftop to something larger than the Nyquist
     frequency (e.g., 999999.9).  To do a lowpass only, set fbot to 0.0.
   - However, the 0 and Nyquist frequencies are always removed.
   - Return value is the number of linear dimensions projected out.
     If 0 is returned, something funky happened.
*//*------------------------------------------------------------------------*/

int THD_bandpass_vectors( int nlen , int nvec   , float **vec ,
                          float dt , float fbot , float ftop  ,
                          int qdet , int nort   , float **ort  )
{
   int nfft,nby2 , iv, jbot,jtop , ndof=0 ; register int jj ;
   float df , tapr ;
   register float *xar, *yar=NULL ;
   register complex *zar ; complex Zero={0.0f,0.0f} ;

ENTRY("THD_bandpass_vectors") ;

   if( ftop > ICOR_MAX_FTOP && qdet < 0 && (nort <= 0 || ort == NULL) )
     RETURN(ndof) ;   /* 26 Feb 2010: do nothing at all? */

   if( nlen < 9 || nvec < 1 || vec == NULL ){
     ERROR_message("bad bandpass data?");
     RETURN(ndof);
   }
   if( bpwrn && dt > 60.0f ) {
     WARNING_message("Your bandpass timestep (%f) is high.\n"
                     "   Make sure units are 'sec', not 'msec'.\n"
                     "   This warning will not be repeated." ,
                     dt);
     bpwrn = 0;
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

   nfft = (nfft_fixed >= nlen) ? nfft_fixed : csfft_nextup_even(nlen) ;
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

   switch( qdet ){
     case 2:
       ndof += 2 ;
       for( iv=0 ; iv < nvec ; iv++ )
         THD_quadratic_detrend( nlen, vec[iv], NULL,NULL,NULL ) ;
     break ;

     case 1:
       ndof += 1 ;
       for( iv=0 ; iv < nvec ; iv++ )
         THD_linear_detrend( nlen, vec[iv], NULL,NULL ) ;
     break ;

     case 0:
       for( iv=0 ; iv < nvec ; iv++ )
         THD_const_detrend( nlen, vec[iv], NULL ) ;
     break ;
   }

   if( qdet > 0 || jbot > 0 || jtop < nby2-1 ){  /* 26 Feb 2010: do the FFTs */

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

   }  /* end of FFTs */

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

/*------------------- 08 Oct 2010: functions for despiking ----------------*/

#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)

#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b)

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
#undef SORT2
#undef SWAP

/*--- get the local median and MAD of values vec[j-4 .. j+4] ---*/

#undef  mead9
#define mead9(j)                                               \
 { float qqq[9] ; int jj = (j)-4 ;                             \
   if( jj < 0 ) jj = 0; else if( jj+8 >= num ) jj = num-9;     \
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

int THD_despike9( int num , float *vec )
{
   int ii , nsp ; float *zma,*zme , med,mad,val ;

   if( num < 9 || vec == NULL ) return 0 ;
   zme = (float *)malloc(sizeof(float)*num) ;
   zma = (float *)malloc(sizeof(float)*num) ;

   for( ii=0 ; ii < num ; ii++ ){
     mead9(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ; free(zma) ;
   if( mad <= 0.0f ){ free(zme); return 0; }  /* should not happen */
   mad *= 6.789f ;  /* threshold value */

   for( nsp=ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ){ vec[ii] = zme[ii]; nsp++; }

   free(zme) ; return nsp ;
}
#undef mead9

/*-------------------------------------------------------------------------*/
/* Return value: .i component = number of vectors with spikes
                 .j component = total number of spikes squashed.
*//*-----------------------------------------------------------------------*/

int_pair THD_vectim_despike9( MRI_vectim *mrv )
{
   int_pair pout = {0,0} ; int nv,ns , ss , kk ;

ENTRY("THD_vectim_despike9") ;

   if( mrv == NULL || mrv->nvals < 9 ) RETURN(pout) ;

   for( nv=ns=kk=0 ; kk < mrv->nvec ; kk++ ){
     ss = THD_despike9( mrv->nvals , VECTIM_PTR(mrv,kk) ) ;
     if( ss > 0 ){ nv++ ; ns += ss ; }
   }

   pout.i = nv ; pout.j = ns ; RETURN(pout) ;
}

/*-------------------------------------------------------------------------*/

THD_3dim_dataset * THD_despike9_dataset( THD_3dim_dataset *inset , byte *mask )
{
   THD_3dim_dataset *outset ;
   MRI_vectim *mrv ;
   int ii ;

ENTRY("THD_despike9_dataset") ;

   if( !ISVALID_DSET(inset) || DSET_NVALS(inset) < 9 ) RETURN(NULL) ;

   mrv = THD_dset_to_vectim(inset,mask,0) ;  DSET_unload(inset) ;
   if( mrv == NULL ) RETURN(NULL) ;

   (void)THD_vectim_despike9(mrv) ;

   outset = EDIT_empty_copy(inset) ;
   for( ii=0 ; ii < DSET_NVALS(outset) ; ii++ )
     EDIT_substitute_brick(outset,ii,MRI_float,NULL) ;

   THD_vectim_to_dset(mrv,outset) ; VECTIM_destroy(mrv) ;
   RETURN(outset) ;
}
