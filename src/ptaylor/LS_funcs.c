#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "mrilib.h"
#include "LS_funcs.h"

#define MIN_WID_PTS (16)  // don't want tiny tiny windows
#define PI_here  (3.141592653589793)

// 0-based counting in this section


// choosing the Welch window at the moment
void MakeWindowVec( float *V, int N)
{
   int i;
   float temp;
   float allwt=0.;

   for( i=0 ; i<N ; i++ ) {
      // WELCH
      //temp = (N-1.)/2.;
      //V[i] = 1 - pow( (i-temp)/temp, 2);

      // Hann
      temp = (2.*PI_here * i)/(N-1);
      V[i] = 0.5*(1-cos(temp));
   }


   // normalize area under weight curve, for any weights:
   for( i=0 ; i<N ; i++ )
      allwt+= V[i]*V[i];
   allwt/= N;
   allwt = sqrt(allwt);
   
   for( i=0 ; i<N ; i++ )
      V[i]/= allwt;

}


// For getting initial length of time series for welch windows.  This
// will be based on numbers of points.  Will need Welch windows of
// constant Npts (so elsewhere we use ofac to maintain a constant
// delta f); conceivably, we could make another function to have
// nonconstant winwids, which is why we do this this way-> for
// generalizability later.
void WelchWindowInfo( float *xpts, int Nx, int Nseg, 
                      int **WInfo, float *WDt, int Nwin )
{
   int i,ii;
   int winwid, Nx_eff;
   int hwin0;                       // starting point of the
                                    // half-offset windows
   INFO_message("Window calculator:");

   winwid = (Nx + Nseg - 1) / Nseg; // *want* int div here! segments
                                    // share one point
   if( winwid < MIN_WID_PTS )
      ERROR_exit("Hey! Make fewer/larger windows! "
                 "Too few points in the Welch win (only %d pts)", 
                 winwid);

   hwin0 = winwid / 2;              // again, int div
   INFO_message("Window width = %d; half-offset = %d", 
                winwid, hwin0);
   //Nx_eff = winwid * Nseg;

   for( i=0 ; i<Nseg ; i++ ) {
      ii = 2*i;
      WInfo[ii][0] = i*(winwid-1);
      WInfo[ii][1] = winwid;            // total number of points in wid,
      if(i < Nseg-1) { // all but last
         WInfo[ii+1][0] = hwin0  + i*(winwid-1);
         WInfo[ii+1][1] = winwid;
      }
   }
   for( i=0 ; i<Nwin ; i++ ) {
      WDt[i] = xpts[WInfo[i][0]+WInfo[i][1]-1] - xpts[WInfo[i][0]];
      INFO_message("[%d, %d] \t-> [%.2f, %.2f] \t-> delta t = %.2f",
                   WInfo[i][0],
                   WInfo[i][0]+WInfo[i][1]-1, 
                   xpts[WInfo[i][0]],
                   xpts[WInfo[i][0]+WInfo[i][1]-1],
                   i,
                   WDt[i]);
   }
   //INFO_message("CHECK!\n\n Nwin = %d,  Nseg = %d", 
   //             Nwin, Nseg);
   INFO_message("Total number of points = %d"
                "\n\t -> goes to %d when assigning windows", 
                Nx, WInfo[Nwin-1][0]+WInfo[Nwin-1][1]);
}



// -------------------->------------->---------->------------>--------

// FFT using GSL's "Radix-2 FFT routines for real data."  This assumes
// that wk1[] and wk2[] are 2^m (m in int) arrays.  Works in place.

/*
  Here, in the PR89* functions, all FOR loops through arrays are in
  that annoying 1-base counting system, for consistency with the
  Fortran description in Press & Rybicki (1989)...

  ... except with the FFT implementation: for this, using GSL's
  "Radix-2 FFT routines for real data."  This assumes that wk1[]
  and wk2[] are 2^m (m in int) arrays.  Works in place.
*/

// modulo/remainder: using essentially Fortran AMOD definition, not:
//   while( a >= b )
//      a -= b;
//      return a;
float PR89_AMOD(float a, float b)
{
   float out=0.;
   int rat=0;

   rat = (int) (a/b);
   out = a - ((float) rat)*b;
   
   return out;
}

// calculate supplementary sizes of arrays and numbers of freqs for
// use in fasper(); pre-calc the N* things, and then input them into
// fasper.
// To avoid some differences in floating point division, am using 
// the fact that hifac = NT/N directly here. -> 
//      hifac * N = NT
void PR89_suppl_calc_Ns( int N, int NT,
                         double ofac, double hifac, 
                         int *Nout, int *Ndim)
{
   int Nfreq, Nfreqt;

   if( NT > 0 ) { // newer
      *Nout = (int) (0.5 * ofac * NT);
      Nfreqt = (int) (ofac * NT * MACC); 
   }
   else { // older
      *Nout = (int) (0.5 * ofac * hifac * N);  
      Nfreqt = (int) (ofac * hifac * N * MACC);     
   }

   Nfreq = 64;
   while (Nfreq < Nfreqt ) 
      Nfreq *= 2;

   *Ndim = 2 * Nfreq;

   //  INFO_message("%d %d %d %d", *Nout, Nfreqt, Nfreq, *Ndim);
}

// here, (Nout, Nwk) are calculated in separate functions, and
// therefore simply input here; also, here, Ndim=Nwk; Nfreq and Nfreqt
// are just temporary intermediate quantities used in the other
// function.  wk1[] and wk2[] are make of doubles because we are using
// GSL for the FFT.  Other differences from PR89, due to different
// functions being used, are noted below.  Some extra features are
// added to control output format (normalizing and/or amplitudizing).
// if winvec==NULL, then don't window; else, window
void PR89_fasper( float *x, 
                  float *y, int N,
                  float *ywin, float *winvec,
                  double ofac, 
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

   /*
   PR89_suppl_avevar(y, N, &AVE, &VAR);
   for( j=1 ; j<=N ; j++ ) 
      ywin[j] = y[j] - AVE; // demean
   // ! windowing
   if(winvec)
      for( j=1 ; j<=N ; j++ ) 
         ywin[j]*=winvec[j];
   if (DO_NORM)
      PR89_suppl_avevar(ywin, N, &AVE, &VAR);
   */

   if(winvec) {
      for( j=1 ; j<=N ; j++ ) 
         ywin[j]= y[j]*winvec[j];
      PR89_suppl_avevar(ywin, N, &AVE, &VAR);
      for( j=1 ; j<=N ; j++ ) 
         ywin[j]-= AVE; // demean
   }
   else{
      PR89_suppl_avevar(y, N, &AVE, &VAR);
      for( j=1 ; j<=N ; j++ ) 
         ywin[j] = y[j] - AVE; // demean
   }
   
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
      PR89_spread(ywin[j], wk1, Ndim, Ck, MACC); 
      PR89_spread(1., wk2, Ndim, Ckk, MACC);
   }

   // Using GSL functions, so have to change what K is for real/imag
   // in NR's realft(); GSL uses zero-base counting internally
   mm = gsl_fft_real_radix2_transform(wk1+1, 1, Ndim);
   mm = gsl_fft_real_radix2_transform(wk2+1, 1, Ndim);

   //for( j=0 ; j<Ndim ; j++ ) 
   //   INFO_message("%d\t %f  ",j,wk2[j]);

   DF = 1./(xdif*ofac);
   //INFO_message("DF: %f     Nout = %d",DF,Nout);
   //INFO_message("mm=%d, FAC=%f, FNDIM=%f",mm,FAC, FNDIM);
   //INFO_message("Ck=%f  Ckk=%f", Ck, Ckk);

   //fprintf(stderr, "\n\n\n TERM COMP ---- \n");

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

      // badness if == 0.  fixed, July2016
      hypo = sqrt(wk2[K]*wk2[K]+wk2[KK]*wk2[KK])+0.000001; 

      hc2wt = 0.5*wk2[K]/hypo;
      hs2wt = -0.5*wk2[KK]/hypo;
      cwt = sqrt(0.5 + hc2wt);
      swt = PR89_SIGN(sqrt(0.5 - hc2wt), hs2wt);
      den = 0.5*N + hc2wt*wk2[K] - hs2wt*wk2[KK];
      cterm = pow((cwt*wk1[K] - swt*wk1[KK]),2)/den;
      sterm = pow((-cwt*wk1[KK] - swt*wk1[K]),2)/(N-den);


      wk1[j] = j*DF;
      wk2[j] = (cterm + sterm)/(2.);
      //fprintf(stderr, " %f, ", (float) wk2[j]);

      if( DO_NORM )
         wk2[j]/= VAR;

      /*if( wk2[j] > PMAX ) {
         PMAX = wk2[j];
         *jmax = j;
         }*/
      
      //if( DO_AMP )   ---> do this later, for greater consistency of power/amp
      //   wk2[j] = sqrt(wk2[j]);

      K++; // also different than in PR89
   }

   /*
   // significance evaluation; have to check later with scaling by N_T
   // done in main function
   expy = exp(-PMAX);
   effm = 2*Nout/ofac; // -> this is ~the scaling in the output...
   *prob = effm*expy;
   if( *prob > 0.01)
      *prob = 1. - pow((1.-expy), effm);
   */

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
      exit(211);
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

// Setup to have matching values with FORTRAN-based counting in
// original
void PR89_spread(float y, double *YY, int N, float x, int M)
{
   // when selecting element of Nfac, use 'M-1' b/c we are in C now,
   // not Fortran --> NOT HERE, NOPE
   int Nfac[11] = {0,1,1,2,6,24,120,720,5040,40320,362880}; 
   int ilo, ihi, nden;
   float fac;
   int j, ix; 

   if(M>10) {
      ERROR_exit("factorial table is too small in PR89_spread.");
      exit(18);
   }

   ix = (int) x;
   if( x == (float) ix) // tests if x (-> Ck or Ckk) is int-valued
      YY[ix]+= y;
   else {
      ilo = PR89_min_int(PR89_max_int( (int) (x-0.5*M+1.),1), N-M+1);
      ihi = ilo + M - 1;
      nden = Nfac[M];
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
