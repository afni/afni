#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "LS_funcs.h"


// FFT using GSL's "Radix-2 FFT routines for real data."  This assumes
// that wk1[] and wk2[] are 2^m (m in int) arrays.  Works in place.

/*
  Here, in the PR89* functions, all FOR loops through arrays are in
  that annoying 1-base counting system, for consistency with the
  Fortran description in Press & Rybicki (1989)...

  ... except with the FFT implementation: for this, using GSL's
  "Radix-2 FFT routines for real data."  This assumes // that wk1[]
  and wk2[] are 2^m (m in int) arrays.  Works in place.
*/

// modulo/remainder
float PR89_AMOD(float a, float b)
{
   while( a >= b )
      a -= b;
   return a;
}

// calculate supplementary sizes of arrays and numbers of freqs for
// use in fasper(); pre-calc the N* things, and then input them into
// fasper
void PR89_suppl_calc_Ns( int N, float ofac, float hifac, 
                         int *Nout, int *Ndim)
{
   int Nfreq, Nfreqt;

   *Nout = (int) (0.5 * ofac * hifac * N);
   Nfreqt = ofac * hifac * N * MACC;
   Nfreq = 64;
   while (Nfreq < Nfreqt ) 
      Nfreq *= 2;

   *Ndim = 2 * Nfreq;
}

// here, (Nout, Nwk) are calculated in separate functions, and
// therefore simply input here; also, here, Ndim=Nwk; Nfreq and Nfreqt
// are just temporary intermediate quantities used in the other
// function.  wk1[] and wk2[] are make of doubles because we are using
// GSL for the FFT.  Other differences from PR89, due to different
// functions being used, are noted below.  Some extra features are
// added to control output format (normalizing and/or amplitudizing).
void PR89_fasper( float *x, float *y, int N,
                  float ofac, float hifac,
                  double *wk1, double *wk2, int Nwk, 
                  int Nout, int *jmax, float *prob,
                  int DO_NORM, int DO_AMP)
{
   int j, mm;
   float AVE=0., VAR=0.;
   float xmin=0., xmax=0., xdif=0.;
   float FAC=0., FNDIM=0., Ck=0., Ckk=0.;
   float DF=0.;
   int K = 2;  // different than in PR89, b/c using GSL RFFT, not
               // realft() for transform
   int KK;
   float PMAX = -1.; 
   float hypo=0., hc2wt=0., hs2wt=0.;
   float cwt=0., swt=0., den=0.;
   float cterm=0., sterm=0.;
   float expy=0.; float effm=0.;
   int Ndim;

   // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

   Ndim = Nwk;

   PR89_suppl_avevar(y, N, &AVE, &VAR);
  
   xmin = x[1];
   xmax = xmin;

   for( j=2 ; j<=N ; j++ ) {
      if( x[j] < xmin )
         xmin = x[j];
      if( x[j] > xmax )
         xmax = x[j];
   }
   xdif = xmax - xmin;

   // empty the workspaces
   for( j=1 ; j<=Ndim ; j++ ) 
      wk1[j] = wk2[j] = 0.;
  
   FAC = Ndim/(xdif*ofac);
   FNDIM = Ndim;
   for( j=1 ; j<=N ; j++ ) {
      Ck = 1. + PR89_AMOD((x[j]-xmin)*FAC, FNDIM);
      Ckk = 1. + PR89_AMOD(2.*(Ck-1.), FNDIM);
      PR89_spread(y[j]-AVE, wk1, Ndim, Ck, MACC);
      PR89_spread(1., wk2, Ndim, Ckk, MACC);
   }

   // Using GSL functions, so have to change what K is for real/imag
   // in NR's realft(); GSL uses zero-base counting internally
   mm = gsl_fft_real_radix2_transform(wk1+1, 1, Ndim);
   mm = gsl_fft_real_radix2_transform(wk2+1, 1, Ndim);

   DF = 1./(xdif*ofac);

   for( j=1 ; j<=Nout ; j++ ) {
      // for imag parts-- different criterion than in PR89, because
      // GSL's RFFT orders freqs differently.
      KK = Ndim - K + 2; 

      // NB, for below: compared to PR89, here wk?[KK] -> -wk?[KK],
      // because, as stated in GSL help manual:
      // << In general there are two possible choices for the sign of
      //    the exponential in the transform/inverse-transform
      //    pair. GSL follows the same convention as fftpack, using a
      //    negative exponential for the forward transform. The
      //    advantage of this convention is that the inverse transform
      //    recreates the original function with simple Fourier
      //    synthesis. Numerical Recipes uses the opposite convention,
      //    a positive exponential in the forward transform. >>
      // and PR89 use the NR convention (understandably...).

      hypo = sqrt(wk2[K]*wk2[K] + wk2[KK]*wk2[KK]);

      hc2wt = 0.5*wk2[K]/hypo;
      hs2wt = -0.5*wk2[KK]/hypo;
      cwt = sqrt(0.5 + hc2wt);
      swt = PR89_SIGN(sqrt(0.5 - hc2wt), hs2wt);
      den = 0.5*N + hc2wt*wk2[K] - hs2wt*wk2[KK];
      cterm = pow((cwt*wk1[K] - swt*wk1[KK]),2)/den;
      sterm = pow((-cwt*wk1[KK] + swt*wk1[K]),2)/(N-den);
      wk1[j] = j*DF;
      wk2[j] = (cterm + sterm)/(2.);

      if( DO_NORM )
         wk2[j]/= VAR;

      if( wk2[j] > PMAX ) {
         PMAX = wk2[j];
         *jmax = j;
      }
      
      if( DO_AMP )
         wk2[j] = 2*sqrt(wk2[j]);

      K++; // also different than in PR89
   }

   // significance evaluation; cheap to calc, so leaving in
   expy = exp(-PMAX);
   effm = 2*Nout/ofac;
   *prob = effm*expy;
   if( *prob > 0.01)
      *prob = 1. - pow((1.-expy), effm);
}

int PR89_min_int(int A, int B)
{
   if( A < B )
      return A;
   else
      return B;
}

int PR89_max_int(int A, int B)
{
   if( A > B )
      return A;
   else
      return B;
}


// supplementary function for calculating mean and variance of a 
void PR89_suppl_avevar(float *x, int N, float *AVE, float *VAR) 
{
   int i;
   double mean = 0., vari = 0.;
  
   if( N < 2 ) {
      ERROR_exit("Too few points in the time series! Need at *least* 2!");
      exit(984);
   }

   for( i=1 ; i<=N ; i++ ) {
      mean+= x[i];
      vari+= x[i]*x[i];
   }
  
   mean/= N;
   vari-= N*mean*mean;
   vari/= N-1;

   *AVE = (float) mean;
   *VAR = (float) vari;
}

void PR89_spread(float y, double *YY, int N, float x, int M)
{
   // when selecting element of Nfac, use 'M-1' b/c we are in C now,
   // not Fortran
   int Nfac[10] = {1,1,2,6,24,120,720,5040,40320,362880}; 
   int ilo, ihi, nden;
   float fac;
   int j, ix; 

   if(M>10) {
      ERROR_exit("factorial table is too small in PR89_spread.");
      exit(518);
   }

   ix = (int) x;
   if( x == (float) ix) // tests if x (-> Ck or Ckk) is int-valued
      YY[ix]+= y;
   else {
      ilo = PR89_min_int(PR89_max_int( (int) (x-0.5*M+1.),1), N-M+1);
      ihi = ilo + M - 1;
      nden = Nfac[M-1];
      fac = x - ilo;
      for( j=ilo+1 ; j<=ihi ; j++ )
         fac*= (x-j);
      YY[ihi]+= y*fac/(nden*(x-ihi));
      for( j=ihi-1 ; j>=ilo ; j--) {
         nden = (nden*(j-ihi))/(j+1-ilo);
         YY[j]+= y*fac/(nden*(x-j));
      }
   }
}

