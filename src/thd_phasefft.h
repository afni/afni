/* How to read this file (3dInSync callers)
   Reentrant Fourier phase randomization; used by -temporal_null phase
   with private worker buffers.
   Read the public contracts in thd_phasefft.h before the implementations.
   These helpers are shared with other AFNI programs: keep ISC-specific policy
   in thd_insync*.c. The call flow is documented in thd_insync.c; ownership,
   dimensions, and failure rules remain defined at each shared API below.
*/
#ifndef THD_PHASEFFT_H
#define THD_PHASEFFT_H

/* Private, reentrant float FFT used by AFNI phase-randomization nulls.
   This is a namespaced wrapper around AFNI's fftn_OMP.c so it can coexist
   with the legacy fftn symbols already present in libmrix. */
extern int THD_fftnf_OMP( int ndim, const int dims[], float re[], float im[],
                          int isign, double scaling ) ;

#endif
