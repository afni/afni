#ifndef THD_PHASEFFT_H
#define THD_PHASEFFT_H

/* Private, reentrant float FFT used by 3dRSA's phase-randomization null.
   This is a namespaced wrapper around AFNI's fftn_OMP.c so it can coexist
   with the legacy fftn symbols already present in libmrix. */
extern int THD_fftnf_OMP( int ndim, const int dims[], float re[], float im[],
                          int isign, double scaling ) ;

#endif
