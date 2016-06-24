#ifndef _LoScA_HEADER_
#define _LoScA_HEADER_

#include <gsl/gsl_fft_real.h> // for RFFTing

#define MACC (4)    // num of interp pts per 1/4 cycle of highest
		              // freq; must be int >0.

#define PR89_SIGN(a,b) ((b) > 0.0 ? fabs(a) : -fabs(a)) // umm, sure.

void WelchWindowInfo( float *xpts, int Nx, int Nseg, 
                      int **WInfo, float *WDt, int Nwin );

void MakeWindowVec( float *V, int N);

// calculate supplementary sizes of arrays and numbers of freqs for
// use in fasper(); pre-calc the N* things, and then input them into
// fasper
void PR89_suppl_calc_Ns( int N, int NT,
                         double ofac, double hifac, 
                         int *Nout, int *Ndim);

// supplementary function for calculating mean and variance of a 
void PR89_suppl_avevar(float *x, int N, float *AVE, float *VAR);

// here, (Nout, Nwk) are calculated in separate functions, and
// therefore simply input here; also, here, Ndim=Nwk; Nfreq and Nfreqt
// are just temporary intermediate quantities used in the other
// function.
void PR89_fasper( float *x, 
                    float *y, int N,
                    float *ywin, float *winvec,
                    double ofac, 
                    double *wk1, double *wk2, int Nwk, 
                    int Nout, int *jmax, float *prob,
                    int DO_NORM, int DO_AMP);

float PR89_AMOD(float a, float b);

void PR89_spread(float y, double *YY, int N, float x, int M);

int PR89_min_int(int A, int B);

int PR89_max_int(int A, int B);

#endif /* _LoSc_HEADER_ */
