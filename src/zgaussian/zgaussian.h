#ifndef _ZGAUSSIAN2_HEADER_
#define _ZGAUSSIAN2_HEADER_

/****************************************************************************
 *
 * Generate Gaussian-distribution numbers via the Ziggurat algorithm
 * of Marsaglia and Tsang. The library of functions for this is by
 * J. Burkhardt (ziggurat.*); it is MIT-licensed and portable.
 *
 * Usage in each AFNI program:
 *   - at top:  #include <stdint.h>
 *              #include "zgaussian.h"
 *   - in file: call zgaussian2_init(rseed) once near the top of
 *              main(), before any call to zgaussian2().
 *   - in file, with parallelization: use zgaussian2_sss() version
 *
 ****************************************************************************/

/*  initialize RNG for zgaussian2() usage  */
extern void  zgaussian2_init( uint32_t seed ) ;

/*  Generate random num from Gaussian distribution N(0,1)  */
extern float zgaussian2( void ) ; 

/*  thread-safe variant of zgaussian2(); caller owns jsr state  */
extern float zgaussian2_sss( uint32_t *jsr ) ;
/*  derive a per-thread seed from the base seed; use with zgaussian2_sss()  */
extern uint32_t zgaussian2_thread_seed( int tid ) ;  

#endif /* _ZGAUSSIAN2_HEADER_ */
