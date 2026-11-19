/* How to read this file (3dInSync callers)
   Reentrant Fourier phase randomization; used by -temporal_null phase
   with private worker buffers.
   Read the public contracts in thd_phasefft.h before the implementations.
   These helpers are shared with other AFNI programs: keep ISC-specific policy
   in thd_insync*.c. The call flow is documented in thd_insync.c; ownership,
   dimensions, and failure rules remain defined at each shared API below.
*/
/* fftn_OMP.c is explicitly designed to be included by a consumer.  Namespace
   its public entry points so linking 3dRSA with libmrix cannot collide with
   the older FFT implementation pulled in by csfft.c. */
#ifdef USE_OMP
#include <omp.h>
#endif
#define fftnf_OMP          THD_fftnf_OMP
#define fftn_nextup_one35  THD_fftn_nextup_one35
#define fftn_nextup_even   THD_fftn_nextup_even
#if defined(__clang__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunused-function"
# pragma clang diagnostic ignored "-Wunused-variable"
#elif defined(__GNUC__)
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wunused-function"
# pragma GCC diagnostic ignored "-Wunused-variable"
#endif
#include "fftn_OMP.c"
#if defined(__clang__)
# pragma clang diagnostic pop
#elif defined(__GNUC__)
# pragma GCC diagnostic pop
#endif
