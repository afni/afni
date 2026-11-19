/*
PJM Wants you to know && How to read this file:

Provide a namespaced, reentrant float FFT for phase-randomization
workflows, coexisting with the legacy FFT symbols in libmrix.

Current program consumers: 3dRSA, 3dInSync.
Start with the types and contracts below; implementations are in thd_phasefft.c.
The callers construct the null draws; this module supplies the transform.
*/
#ifndef THD_PHASEFFT_H
#define THD_PHASEFFT_H

/* Private, reentrant float FFT used by AFNI phase-randomization nulls.
   This is a namespaced wrapper around AFNI's fftn_OMP.c so it can coexist
   with the legacy fftn symbols already present in libmrix. */
extern int THD_fftnf_OMP( int ndim, const int dims[], float re[], float im[],
                          int isign, double scaling ) ;

#endif
